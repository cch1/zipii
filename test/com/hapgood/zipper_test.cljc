(ns com.hapgood.zipper-test
  (:require [com.hapgood.zipper :refer :all]
            [clojure.test :refer [deftest is]])
  (:refer-clojure :exclude (replace remove next)))

;; SeqZipper
(deftest list-zip-Zipper
  (let [t '(0 1 2 (20 21) 3 (30 31 (310)))]
    (is (= t (tree (list-zip t))))
    (is (branch? (list-zip t)))
    (is (= t (children (list-zip t))))
    (is (= t (make-tree (list-zip t) () t)))
    (is (nil? (parent (list-zip t))))))

(deftest list-zip-hierarchical-navigation
  (let [grandchild '((1111) 222)
        child (list grandchild 22)
        t (list child 2)
        z (list-zip t)]
    (is (= child (-> z down down parent tree)))
    (is (empty? (path z)))
    (is (= child (-> z down tree)))
    (is (= grandchild (-> z down down tree)))
    (is (= t (-> z down up tree)))
    (is (= t (-> z down down root tree)))
    (is (= [t child] (-> z down down path)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down down down down down)))))

(deftest list-zip-ordered-navigation
  (let [t (list 1 2 3 4)
        z (list-zip t)]
    (is (nil? (-> z right)))
    (is (nil? (-> z left)))
    (is (= 2 (-> z down right tree)))
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

(deftest list-zip-update
  (let [t '(1 2 (31 32) 4)
        z (list-zip t)]
    (is (= '(1 2 (30 31 32) 4) (-> z down right right down (insert-left 30) root tree)))
    (is (= '(1 2 (31 32 33) 4) (-> z down right right down right (insert-right 33) root tree)))
    (is (thrown? Exception (-> z (insert-right 33))))
    (is (thrown? Exception (-> z (insert-left 33))))
    (is (= '(1 2 3 4) (-> z down right right (replace 3) root tree)))
    (is (= '(0 2 (31 32) 4) (-> z down (edit dec) root tree)))
    (is (= '(0 1 2 (31 32) 4) (-> z (insert-child 0) root tree)))
    (is (= '(1) (-> (list-zip '()) (insert-child 1) tree)))
    (is (= '(1 2 (31 32) 4 5) (-> z (append-child 5) root tree)))
    (is (= '(1) (-> (list-zip '()) (append-child 1) tree)))))

(deftest list-zip-iterate
  (let [t '(1 2 (31 32) 4)
        z (list-zip t)
        step (iterate next z)]
    (is (not (end? z)))
    (is (nil? (prev z)))
    (is (= 1 (-> z next tree)))
    (is (= '(31 32) (-> z next next next tree)))
    (is (= 31 (-> z next next next next tree)))
    (is (= 2 (-> z next next next next prev prev tree)))
    (is (-> z next next next next next next next end?))
    (is (end? (nth step 100)))))

(deftest list-zip-remove
  (let [t '(1 2 (31 32) 4)
        z (list-zip t)]
    (is (= '(1 2 4) (-> z down right right remove root tree)))
    (is (thrown? Exception (-> z remove)))
    (is (= '(1 2 () 4) (-> z down right right down right remove remove root tree)))
    (is (= `(1 2 4) (-> z down right right down right remove remove remove root tree)))
    ;; This next one exposes a bug in clojure.zip...
    (is (= () (-> (list-zip '(0)) down remove tree)))))

;; MapZipper
(deftest map-zip-Zipper
  (let [t {:a 1 :b 2 :c {:d 3 :e 4}}]
    (is (= t (val (tree (map-zip t)))))
    (is (= [::root t] (tree (map-zip ::root t))))
    (is (branch? (map-zip t)))
    (is (= t (children (map-zip t))))
    #_ (is (= t (let [nzip (map-zip t)] (make-tree nzip (tree t) t))))
    (is (nil? (parent (map-zip root))))))

(deftest map-zip-hierarchical-navigation
  (let [grandchild {:c 3}
        child {:b grandchild}
        t {:a child}
        z (map-zip ::root t)]
    (is (= [:a child] (-> z down down parent tree)))
    (is (empty? (path z)))
    (is (= [:a child] (-> z down tree)))
    (is (= [:b grandchild] (-> z down down tree)))
    (is (= t (val (-> z down up tree))))
    (is (= t (val (-> z down down root tree))))
    (is (= [[::root t] [:a child]] (-> z down down path)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down down down down)))))

(deftest map-zip-ordered-navigation
  (let [t {:a 1 :b 2 :c 3 :d 4}
        z (map-zip ::root t)]
    (is (nil? (-> z right)))
    (is (nil? (-> z left)))
    (is (= [:b 2] (-> z down right tree)))
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
    (is (= [::root {:a 1 :b 2 :c {:c0 30 :c1 31 :c2 32} :f 5}] (-> z down right right down (insert-left [:c0 30]) root tree)))
    (is (= [::root {:a 1 :b 2 :c {:c1 31 :c2 32 :c3 33} :f 5}] (-> z down right right down right (insert-right [:c3 33]) root tree)))
    (is (thrown? Exception (-> z (insert-right [:z 33]))))
    (is (thrown? Exception (-> z (insert-left [:z 33]))))
    (is (= [::root {:a 1 :b 2 :c 3 :f 5}] (-> z down right right (replace [:c 3]) root tree)))
    (is (= [::root {:a 0 :b 2 :c {:c1 31 :c2 32} :f 5}] (-> z down (edit #(update % 1 dec)) root tree)))
    (is (= [::root {:z 0 :a 1 :b 2 :c {:c1 31 :c2 32} :f 5}] (-> z (insert-child [:z 0]) root tree)))
    (is (= [::root {:a 1}] (-> (map-zip ::root {}) (insert-child [:a 1]) tree)))
    (is (= [::root {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5 :z 9}] (-> z (append-child [:z 9]) root tree)))
    (is (= [::root {:a 1}] (-> (map-zip ::root {}) (append-child [:a 1]) tree)))))

(deftest map-zip-iterate
  (let [t {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5}
        z (map-zip ::root t)
        step (iterate next z)]
    (is (not (end? z)))
    (is (nil? (prev z)))
    (is (= [:a 1] (-> z next tree)))
    (is (= [:c {:c1 31 :c2 32}] (-> z next next next tree)))
    (is (= [:c1 31] (-> z next next next next tree)))
    (is (= [:b 2] (-> z next next next next prev prev tree)))
    (is (-> z next next next next next next next end?))
    (is (end? (nth step 100)))))

(deftest map-zip-remove
  (let [t {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5}
        z (map-zip ::root t)]
    (is (= [::root {:a 1 :b 2 :f 5}] (-> z down right right remove root tree)))
    (is (thrown? Exception (-> z remove)))
    (is (= [::root {:a 1 :b 2 :c {} :f 5}] (-> z down right right down right remove remove root tree)))
    (is (= [::root {:a 1 :b 2 :f 5}] (-> z down right right down right remove remove remove root tree)))
    (is (= [::root {}] (-> (map-zip ::root {:a 1}) down remove tree)))))

;; VecZipper
(deftest vector-zip-Zipper
  (let [t [0 1 2 [20 21] 3 [30 31 [310]]]]
    (is (= t (tree (vector-zip t))))
    (is (branch? (vector-zip t)))
    (is (= t (children (vector-zip t))))
    (is (= t (make-tree (vector-zip t) [] t)))
    (is (nil? (parent (vector-zip t))))))

(deftest vector-zip-hierarchical-navigation
  (let [grandchild [[1111] 222]
        child [grandchild 22]
        t [child 2]
        z (vector-zip t)]
    (is (= child (-> z down down parent tree)))
    (is (empty? (path z)))
    (is (= child (-> z down tree)))
    (is (= grandchild (-> z down down tree)))
    (is (= t (-> z down up tree)))
    (is (= t (-> z down down root tree)))
    (is (= [t child] (-> z down down path)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down down down down down)))))

(deftest vector-zip-ordered-navigation
  (let [t [1 2 3 4]
        z (vector-zip t)]
    (is (nil? (-> z right)))
    (is (nil? (-> z left)))
    (is (= 2 (-> z down right tree)))
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
    (is (= [1 2 [30 31 32] 4] (-> z down right right down (insert-left 30) root tree)))
    (is (= [1 2 [31 32 33] 4] (-> z down right right down right (insert-right 33) root tree)))
    (is (thrown? Exception (-> z (insert-right 33))))
    (is (thrown? Exception (-> z (insert-left 33))))
    (is (= [1 2 3 4] (-> z down right right (replace 3) root tree)))
    (is (= [0 2 [31 32] 4] (-> z down (edit dec) root tree)))
    (is (= [0 1 2 [31 32] 4] (-> z (insert-child 0) root tree)))
    (is (= [1] (-> (list-zip '()) (insert-child 1) tree)))
    (is (= [1 2 [31 32] 4 5] (-> z (append-child 5) root tree)))
    (is (= [1] (-> (list-zip []) (append-child 1) tree)))))

(deftest vector-zip-iterate
  (let [t [1 2 [31 32] 4]
        z (vector-zip t)
        step (iterate next z)]
    (is (not (end? z)))
    (is (nil? (prev z)))
    (is (= 1 (-> z next tree)))
    (is (= [31 32] (-> z next next next tree)))
    (is (= 31 (-> z next next next next tree)))
    (is (= 2 (-> z next next next next prev prev tree)))
    (is (-> z next next next next next next next end?))
    (is (end? (nth step 100)))))

(deftest vector-zip-remove
  (let [t [1 2 [31 32] 4]
        z (vector-zip t)]
    (is (= [1 2 4] (-> z down right right remove root tree)))
    (is (thrown? Exception (-> z remove)))
    (is (= [1 2 [] 4] (-> z down right right down right remove remove root tree)))
    (is (= [1 2 4] (-> z down right right down right remove remove remove root tree)))
    (is (= [] (-> (vector-zip [0]) down remove tree)))))

;; index/key navigation
(deftest map-zip-key-navigation
  (let [grandchild {:a00 130 :a01 131}
        child {:a0 10 :a1 11 :a2 12 :a3 grandchild :a4 14}
        t {:a child :b 2}
        z (map-zip ::root t)]
    (is (= [:a child] (-> z (down-to :a) tree)))
    (is (= [:a00 130] (-> z (down-to :a) (down-to :a3) (down-to :a00) tree)))
    (is (nil? (-> z (down-to :c))))))

(deftest vector-zip-index-navigation
  (let [grandchild [130 131]
        child [10 11 12 grandchild 14]
        t [child 2]
        z (vector-zip t)]
    (is (= child (-> z (down-to 0) tree)))
    (is (= 130 (-> z (down-to 0) (down-to 3) (down-to 0) tree)))
    (is (nil? (-> z (down-to 9))))))

;; Edge case exploration
(deftest move-and-edit
  (let [t [1 1 3 [4 5]]]
    (is (= (-> t vector-zip down right (edit inc) root tree)
           (-> t vector-zip down right (edit inc) right root tree)))
    (is (= (-> t vector-zip down right right (edit inc) root tree)
           (-> t vector-zip down right right (edit inc) left root tree)))
    (is (= (-> t vector-zip down rightmost left (edit inc) root tree)
           (-> t vector-zip down right right (edit inc) left root tree)))))

(deftest preserve-type-on-edit
  (let [t (sorted-set 1 2 3)]
    (is (instance? (class t)
                   (-> t seqable-zip down right (edit dec) root tree))))
  (let [t (list 1 2 3)]
    (is (instance? (class t)
                   (-> t list-zip down right (edit dec) root tree))))
  (let [t (sorted-map :a 1 :b 2 :c 3)]
    (is (instance? (class t)
                   (-> t map-zip down right (edit #(update % 1 dec)) root tree val))))
  (let [t (vector-of :long 1 2 3)]
    (is (instance? (class t)
                   (-> t vector-zip down right (edit inc) root tree)))))
