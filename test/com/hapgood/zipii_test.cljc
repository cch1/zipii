(ns com.hapgood.zipii-test
  (:refer-clojure :exclude (replace remove next))
  (:require [com.hapgood.zipii :refer [->me
                                       up down left right rightmost leftmost next prev end? nth-child root path
                                       insert-left insert-right insert-child append-down append-child replace edit remove
                                       seqable-zip seq-zip coll-zip list-zip vector-zip map-zip map-zip* xml-zip
                                       node children]]
            [com.hapgood.zipper.loc :refer [loc?]]
            [clojure.test :refer [deftest is are]])
  (:import #?(:clj (clojure.lang ExceptionInfo))))

(deftest access-move-query
  (let [t '(1 (21 22) 3)
        z (list-zip t)]
    (is (= t (-> z node)))
    (is (= t (-> z children)))
    (is (= 1 (-> z down node)))
    (is (= '(21 22) (-> z down right node)))
    (is (= 1 (-> z down right left node)))
    (is (= t (-> z down up node)))))

(deftest extended-move
  (let [t '(1 (21 22) 3)
        z (list-zip t)]
    (is (= t (-> z down right down root)))
    (is (= [t '(21 22)] (-> z down right down path)))
    (is (= 3 (-> z down rightmost node)))
    (is (= 1 (-> z down leftmost node)))))

(deftest mutate
  (let [t '(1 (21) 3)
        z (list-zip t)]
    (let [z (-> z down right (replace 2))]
      (is (= 2 (-> z node)))
      (is (= '(1 2 3) (-> z root))))
    (let [z (-> z down (insert-left 0))]
      (is (= 1 (-> z node)))
      (is (= '(0 1 (21) 3) (-> z root))))
    (let [z (-> z down (insert-right 1.5))]
      (is (= 1 (-> z node)))
      (is (= '(1 1.5 (21) 3) (-> z root))))))

(deftest extended-mutate
  (let [t '(1 (21 22) 3)
        z (list-zip t)]
    (let [z (-> z down (edit dec))]
      (is (= 0 (-> z node)))
      (is (= '(0 (21 22) 3) (-> z root))))
    (let [z (-> z (append-down 4))]
      (is (= 4 (-> z node)))
      (is (= '(1 (21 22) 3 4) (-> z root))))))

(deftest iterative-move
  (let [t '(1 2 (31 32) 4)
        z (list-zip t)
        step (iterate next z)]
    (is (not (end? z)))
    (is (nil? (prev z)))
    (is (= 1 (-> z next node)))
    (is (= '(31 32) (-> z next next next node)))
    (is (= 31 (-> z next next next next node)))
    (is (= 2 (-> z next next next next prev prev node)))
    (is (-> z next next next next next next next end?))
    (is (end? (nth step 100)))))

;; Test invariants
(defn pair [x f] [x (f x)])

(def degenerate-zippers
  [(pair true seqable-zip) ; watch out, nil is seqable
   (pair nil coll-zip)
   (pair nil seq-zip)
   (pair nil list-zip)
   (pair (->me [::root nil]) map-zip*)
   (pair nil vector-zip)
   (pair "" xml-zip)])

(def empty-zippers
  [(pair () seqable-zip)
   (pair () coll-zip)
   (pair () seq-zip)
   (pair () list-zip)
   (pair (->me [::root {}]) map-zip*)
   (pair [] vector-zip)
   (pair {:content []} xml-zip)])

(def singleton-zippers
  [(pair '(1) seqable-zip)
   (pair '(1) coll-zip)
   (pair '(1) seq-zip)
   (pair '(1) list-zip)
   (pair (->me [::root {:a 1}]) map-zip*)
   (pair [1] vector-zip)
   (pair {:content [{:tag :a :content []}]} xml-zip)])

(deftest invariant-access-move-query
  (doseq [[t z] degenerate-zippers]
    (is (= t (-> z node)))
    (is (nil? (-> z left)))
    (is (nil? (-> z right)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down)))
    (is (= z (-> z leftmost)))
    (is (= z (-> z rightmost)))
    (is (-> z next end?))
    (is (nil? (-> z prev))))
  (doseq [[t z] empty-zippers]
    (is (empty? (-> z children)))
    (is (nil? (-> z left)))
    (is (nil? (-> z right)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down)))
    (is (= z (-> z leftmost)))
    (is (= z (-> z rightmost)))
    (is (-> z next end?))
    (is (nil? (-> z prev))))
  (doseq [[t z] singleton-zippers]
    (is (not (empty? (-> z children))))
    (is (loc? (-> z down)))
    (is (-> z next next end?))
    (is (= z (-> z next prev)))))

(deftest invariant-mutate
  (doseq [[t z] degenerate-zippers]
    (is (thrown? ExceptionInfo (-> z remove)))
    (is (thrown? ExceptionInfo (-> z (insert-right nil))))
    (is (thrown? ExceptionInfo (-> z (insert-left nil))))
    (is (thrown? ExceptionInfo (-> z (insert-child nil)))))
  (doseq [[t z] empty-zippers]
    (is (thrown? ExceptionInfo (-> z remove)))
    (is (thrown? ExceptionInfo (-> z (insert-right nil))))
    (is (thrown? ExceptionInfo (-> z (insert-left nil))))
    (is (loc? (-> z (insert-child [:a 1]))))
    (is (loc? (-> [] vector-zip (insert-child [:a 1])))))
  (doseq [[t z] singleton-zippers]
    (is (= t (let [z (-> z down)
                   item (-> z node)]
               (-> z remove (insert-child item) root))))))

(deftest nth-child-porcelain
  (is (= 4 (-> (range) seq-zip (nth-child 5) node))))

;; Tests for specific zipper variants

(deftest list-zip-hierarchical-navigation
  (let [grandchild '((1111) 222)
        child (list grandchild 22)
        t (list child 2)
        z (list-zip t)]
    (is (= child (-> z down node)))
    (is (= grandchild (-> z down down node)))
    (is (= [t child] (-> z down down path)))))

(deftest list-zip-ordered-navigation
  (let [t (list 1 2 3 4)
        z (list-zip t)]
    (is (nil? (-> z right)))
    (is (nil? (-> z left)))
    (is (= 2 (-> z down right node)))
    (is (nil? (-> z down right right right right)))
    (is (= (-> z down) (-> z down right left)))
    (is (nil? (-> z down left)))))

(deftest list-zip-ordered-extra-navigation
  (let [t (list 1 2 3 4)
        z (list-zip t)]
    (is (= (-> z down right right right)
           (-> z down right right right rightmost)
           (-> z down rightmost)
           (-> z down rightmost rightmost)))
    (is (= (-> z down right right right leftmost)
           (-> z down leftmost)
           (-> z down leftmost leftmost)))))

(deftest list-zip-update
  (let [t '(1 2 (31 32) 4)
        z (list-zip t)]
    (is (= '(1 2 (30 31 32) 4) (-> z down right right down (insert-left 30) root)))
    (is (= '(1 2 (31 32 33) 4) (-> z down right right down right (insert-right 33) root)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (-> z (insert-right 33))))
    (is (thrown? #?(:clj Exception :cljs js/Error) (-> z (insert-left 33))))
    (is (= '(1 2 3 4) (-> z down right right (replace 3) root)))
    (is (= '(0 2 (31 32) 4) (-> z down (edit dec) root)))
    (is (= '(0 1 2 (31 32) 4) (-> z (insert-child 0) root)))
    (is (= '(1) (-> (list-zip '()) (insert-child 1) node)))
    (is (= '(1 2 (31 32) 4 5) (-> z (append-child 5) root)))
    (is (= '(1) (-> (list-zip '()) (append-child 1) node)))))

(deftest list-zip-iterate
  (let [t '(1 2 (31 32) 4)
        z (list-zip t)
        step (iterate next z)]
    (is (not (end? z)))
    (is (nil? (prev z)))
    (is (= 1 (-> z next node)))
    (is (= '(31 32) (-> z next next next node)))
    (is (= 31 (-> z next next next next node)))
    (is (= 2 (-> z next next next next prev prev node)))
    (is (-> z next next next next next next next end?))
    (is (end? (nth step 100)))))

(deftest list-zip-remove
  (let [t '(1 2 (31 32) 4)
        z (list-zip t)]
    (is (= '(1 2 4) (-> z down right right remove root)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (-> z remove)))
    (is (= '(1 2 () 4) (-> z down right right down right remove remove root)))
    (is (= `(1 2 4) (-> z down right right down right remove remove remove root)))
    ;; This next one exposes a bug in clojure.zip...
    (is (= () (-> (list-zip '(0)) down remove node)))))

;; MapZipper
(deftest map-zip-hierarchical-navigation
  (let [grandchild {:c 3}
        child {:b grandchild}
        t {:a child}
        z (map-zip ::root t)]
    (is (= [:a child] (-> z down down up node)))
    (is (empty? (path z)))
    (is (= [:a child] (-> z down node)))
    (is (= [:b grandchild] (-> z down down node)))
    (is (= t (val (-> z down up node))))
    (is (= t (val (-> z down down root))))
    (is (= [[::root t] [:a child]] (-> z down down path)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down down down down)))))

(deftest map-zip-ordered-navigation
  (let [t {:a 1 :b 2 :c 3 :d 4}
        z (map-zip ::root t)]
    (is (nil? (-> z right)))
    (is (nil? (-> z left)))
    (is (= [:b 2] (-> z down right node)))
    (is (nil? (-> z down right right right right)))
    (is (= (-> z down) (-> z down right left)))
    (is (nil? (-> z down left)))
    (is (= (-> z down right right right)
           (-> z down right right right rightmost)
           (-> z down rightmost)
           (-> z down rightmost rightmost)))
    (is (= (-> z down right right right leftmost)
           (-> z down leftmost)
           (-> z down leftmost leftmost)))))

(deftest map-zip-update
  (let [t {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5}
        z (map-zip ::root t)]
    (is (= [::root {:a 1 :b 2 :c {:c0 30 :c1 31 :c2 32} :f 5}] (-> z down right right down (insert-left [:c0 30]) root)))
    (is (= [::root {:a 1 :b 2 :c {:c1 31 :c2 32 :c3 33} :f 5}] (-> z down right right down right (insert-right [:c3 33]) root)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (-> z (insert-right [:z 33]))))
    (is (thrown? #?(:clj Exception :cljs js/Error) (-> z (insert-left [:z 33]))))
    (is (= [::root {:a 1 :b 2 :c 3 :f 5}] (-> z down right right (replace [:c 3]) root)))
    (is (= [::root {:a 0 :b 2 :c {:c1 31 :c2 32} :f 5}] (-> z down (edit #(update % 1 dec)) root)))
    (is (= [::root {:z 0 :a 1 :b 2 :c {:c1 31 :c2 32} :f 5}] (-> z (insert-child [:z 0]) root)))
    (is (= [::root {:a 1}] (-> (map-zip ::root {}) (insert-child [:a 1]) root)))
    (is (= [::root {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5 :z 9}] (-> z (append-child [:z 9]) root)))
    (is (= [::root {:a 1}] (-> (map-zip ::root {}) (append-child [:a 1]) root)))))

(deftest map-zip-iterate
  (let [t {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5}
        z (map-zip ::root t)
        step (iterate next z)]
    (is (not (end? z)))
    (is (nil? (prev z)))
    (is (= [:a 1] (-> z next node)))
    (is (= [:c {:c1 31 :c2 32}] (-> z next next next node)))
    (is (= [:c1 31] (-> z next next next next node)))
    (is (= [:b 2] (-> z next next next next prev prev node)))
    (is (-> z next next next next next next next end?))
    (is (end? (nth step 100)))))

(deftest map-zip-remove
  (let [t {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5}
        z (map-zip ::root t)]
    (is (= [::root {:a 1 :b 2 :f 5}] (-> z down right right remove root)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (-> z remove)))
    (is (= [::root {:a 1 :b 2 :c {} :f 5}] (-> z down right right down right remove remove root)))
    (is (= [::root {:a 1 :b 2 :f 5}] (-> z down right right down right remove remove remove root)))
    (is (= [::root {}] (-> (map-zip ::root {:a 1}) down remove node)))))

;; VecZipper
(deftest vector-zip-Zipper
  (let [t [0 1 2 [20 21] 3 [30 31 [310]]]]
    (is (= t (node (vector-zip t))))
    (is (= t (children (vector-zip t))))))

(deftest vector-zip-hierarchical-navigation
  (let [grandchild [[1111] 222]
        child [grandchild 22]
        t [child 2]
        z (vector-zip t)]
    #_ (is (= child (-> z down down parent node)))
    (is (empty? (path z)))
    (is (= child (-> z down node)))
    (is (= grandchild (-> z down down node)))
    (is (= t (-> z down up node)))
    (is (= t (-> z down down root)))
    (is (= [t child] (-> z down down path)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down down down down down)))))

(deftest vector-zip-ordered-navigation
  (let [t [1 2 3 4]
        z (vector-zip t)]
    (is (nil? (-> z right)))
    (is (nil? (-> z left)))
    (is (= 2 (-> z down right node)))
    (is (nil? (-> z down right right right right)))
    (is (= (-> z down) (-> z down right left)))
    (is (nil? (-> z down left)))
    (is (= (-> z down right right right)
           (-> z down right right right rightmost)
           (-> z down rightmost)
           (-> z down rightmost rightmost)))
    (is (= (-> z down right right right leftmost)
           (-> z down leftmost)
           (-> z down leftmost leftmost)))))

(deftest vector-zip-update
  (let [t [1 2 [31 32] 4]
        z (vector-zip t)]
    (is (= [1 2 [30 31 32] 4] (-> z down right right down (insert-left 30) root)))
    (is (= [1 2 [31 32 33] 4] (-> z down right right down right (insert-right 33) root)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (-> z (insert-right 33))))
    (is (thrown? #?(:clj Exception :cljs js/Error) (-> z (insert-left 33))))
    (is (= [1 2 3 4] (-> z down right right (replace 3) root)))
    (is (= [0 2 [31 32] 4] (-> z down (edit dec) root)))
    (is (= [0 1 2 [31 32] 4] (-> z (insert-child 0) root)))
    (is (= [1] (-> (list-zip '()) (insert-child 1) node)))
    (is (= [1 2 [31 32] 4 5] (-> z (append-child 5) root)))))

(deftest vector-zip-iterate
  (let [t [1 2 [31 32] 4]
        z (vector-zip t)
        step (iterate next z)]
    (is (not (end? z)))
    (is (nil? (prev z)))
    (is (= 1 (-> z next node)))
    (is (= [31 32] (-> z next next next node)))
    (is (= 31 (-> z next next next next node)))
    (is (= 2 (-> z next next next next prev prev node)))
    (is (-> z next next next next next next next end?))
    (is (end? (nth step 100)))))

(deftest vector-zip-remove
  (let [t [1 2 [31 32] 4]
        z (vector-zip t)]
    (is (= [1 2 4] (-> z down right right remove root)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (-> z remove)))
    (is (= [1 2 [] 4] (-> z down right right down right remove remove root)))
    (is (= [1 2 4] (-> z down right right down right remove remove remove root)))
    (is (= [] (-> (vector-zip [0]) down remove node)))))

;; Edge case exploration
(deftest move-and-edit
  (let [t [1 1 3 [4 5]]]
    (is (= (-> t vector-zip down right (edit inc) root)
           (-> t vector-zip down right (edit inc) right root)))
    (is (= (-> t vector-zip down right right (edit inc) root)
           (-> t vector-zip down right right (edit inc) left root)))
    (is (= (-> t vector-zip down rightmost left (edit inc) root)
           (-> t vector-zip down right right (edit inc) left root)))))

(deftest preserve-type-on-edit
  (let [t (sorted-set 1 2 3)]
    (is (instance? (type t)
                   (-> t coll-zip down right (edit dec) root))))
  (let [t (list 1 2 3)]
    (is (instance? (type t)
                   (-> t list-zip down right (edit dec) root))))
  (let [t (sorted-map :a 1 :b 2 :c 3)]
    (is (instance? (type t)
                   (-> t map-zip down right (edit #(update % 1 dec)) root val))))
  #?(:clj (let [t (vector-of :long 1 2 3)]
            (is (instance? (type t)
                           (-> t vector-zip down right (edit inc) root))))))

;; RH compatibility shims
(deftest replace-shim
  (let [t [1 2 3 [4 5]]
        z (vector-zip t)]
    (let [z (-> z down right right right down (replace 4.5))]
      (is (= 4.5 (-> z node)))
      (is (= [1 2 3 [4.5 5]] (-> z root))))))

(deftest remove-shim
  (let [t [1 2 3 [4 5]]
        z (vector-zip t)]
    (let [z (-> z down right right right down remove)]
      (is (= [5] (-> z node)))
      (is (= [1 2 3 [5]] (-> z root))))))

(deftest insert-child-shim
  (let [t [1 2 3 [4 5]]
        z (vector-zip t)]
    (let [z (-> z down right right right (insert-child 3.5))]
      (is (= [3.5 4 5] (-> z node)))
      (is (= [1 2 3 [3.5 4 5]] (-> z root))))))

(deftest append-child-shim
  (let [t [1 2 3 [4 5]]
        z (vector-zip t)]
    (let [z (-> z down right right right (append-child 6))]
      (is (= [4 5 6] (-> z node)))
      (is (= [1 2 3 [4 5 6]] (-> z root))))))

#?(:clj (deftest serialize
          (doseq [[t z] singleton-zippers]
            (is (= z (-> z next pr-str read-string prev))))))
