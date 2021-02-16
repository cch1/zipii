(ns com.hapgood.zipper-test
  (:require [com.hapgood.zipper :refer :all]
            [clojure.zip :as z]
            [clojure.test :refer [deftest is]])
  (:refer-clojure :exclude (replace remove next)))

;; SeqZipper
(deftest list-zip-Zipper
  (let [nroot '(0 1 2 (20 21) 3 (30 31 (310)))]
    (is (= nroot (node (list-zip nroot))))
    (is (branch? (list-zip nroot)))
    (is (= nroot (children (list-zip nroot))))
    (is (= nroot (make-node (list-zip nroot) () nroot)))
    (is (nil? (parent (list-zip nroot))))))

(deftest list-zip-hierarchical-navigation
  (let [grandchild '((1111) 222)
        child (list grandchild 22)
        nroot (list child 2)
        z (list-zip nroot)]
    (is (= child (-> z down down parent node)))
    (is (empty? (path z)))
    (is (= child (-> z down node)))
    (is (= grandchild (-> z down down node)))
    (is (= nroot (-> z down up node)))
    (is (= nroot (-> z down down root node)))
    (is (= [nroot child] (-> z down down path)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down down down down down)))))

(deftest list-zip-ordered-navigation
  (let [nroot (list 1 2 3 4)
        z (list-zip nroot)]
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

(deftest list-zip-update
  (let [nroot '(1 2 (31 32) 4)
        z (list-zip nroot)]
    (is (= '(1 2 (30 31 32) 4) (-> z down right right down (insert-left 30) root node)))
    (is (= '(1 2 (31 32 33) 4) (-> z down right right down right (insert-right 33) root node)))
    (is (thrown? Exception (-> z (insert-right 33))))
    (is (thrown? Exception (-> z (insert-left 33))))
    (is (= '(1 2 3 4) (-> z down right right (replace 3) root node)))
    (is (= '(0 2 (31 32) 4) (-> z down (edit dec) root node)))
    (is (= '(0 1 2 (31 32) 4) (-> z (insert-child 0) root node)))
    (is (= '(1) (-> (list-zip '()) (insert-child 1) node)))
    (is (= '(1 2 (31 32) 4 5) (-> z (append-child 5) root node)))
    (is (= '(1) (-> (list-zip '()) (append-child 1) node)))))

(deftest list-zip-iterate
  (let [nroot '(1 2 (31 32) 4)
        z (list-zip nroot)
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
  (let [nroot '(1 2 (31 32) 4)
        z (list-zip nroot)]
    (is (= '(1 2 4) (-> z down right right remove root node)))
    (is (thrown? Exception (-> z remove)))
    (is (= '(1 2 () 4) (-> z down right right down right remove remove root node)))
    (is (= `(1 2 4) (-> z down right right down right remove remove remove root node)))
    ;; This next one exposes a bug in clojure.zip...
    (is (= () (-> (list-zip '(0)) down remove node)))))

;; MapZipper
(deftest map-zip-Zipper
  (let [nroot {:a 1 :b 2 :c {:d 3 :e 4}}]
    (is (= nroot (val (node (map-zip nroot)))))
    (is (= [::root nroot] (node (map-zip ::root nroot))))
    (is (branch? (map-zip nroot)))
    (is (= nroot (children (map-zip nroot))))
    #_ (is (= nroot (let [nzip (map-zip nroot)] (make-node nzip (node nroot) nroot))))
    (is (nil? (parent (map-zip nroot))))))

(deftest map-zip-hierarchical-navigation
  (let [grandchild {:c 3}
        child {:b grandchild}
        nroot {:a child}
        z (map-zip ::root nroot)]
    (is (= [:a child] (-> z down down parent node)))
    (is (empty? (path z)))
    (is (= [:a child] (-> z down node)))
    (is (= [:b grandchild] (-> z down down node)))
    (is (= nroot (val (-> z down up node))))
    (is (= nroot (val (-> z down down root node))))
    (is (= [[::root nroot] [:a child]] (-> z down down path)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down down down down)))))

(deftest map-zip-ordered-navigation
  (let [nroot {:a 1 :b 2 :c 3 :d 4}
        z (map-zip ::root nroot)]
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
  (let [nroot {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5}
        z (map-zip ::root nroot)]
    (is (= [::root {:a 1 :b 2 :c {:c0 30 :c1 31 :c2 32} :f 5}] (-> z down right right down (insert-left [:c0 30]) root node)))
    (is (= [::root {:a 1 :b 2 :c {:c1 31 :c2 32 :c3 33} :f 5}] (-> z down right right down right (insert-right [:c3 33]) root node)))
    (is (thrown? Exception (-> z (insert-right [:z 33]))))
    (is (thrown? Exception (-> z (insert-left [:z 33]))))
    (is (= [::root {:a 1 :b 2 :c 3 :f 5}] (-> z down right right (replace [:c 3]) root node)))
    (is (= [::root {:a 0 :b 2 :c {:c1 31 :c2 32} :f 5}] (-> z down (edit #(update % 1 dec)) root node)))
    (is (= [::root {:z 0 :a 1 :b 2 :c {:c1 31 :c2 32} :f 5}] (-> z (insert-child [:z 0]) root node)))
    (is (= [::root {:a 1}] (-> (map-zip ::root {}) (insert-child [:a 1]) node)))
    (is (= [::root {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5 :z 9}] (-> z (append-child [:z 9]) root node)))
    (is (= [::root {:a 1}] (-> (map-zip ::root {}) (append-child [:a 1]) node)))))

(deftest map-zip-iterate
  (let [nroot {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5}
        z (map-zip ::root nroot)
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
  (let [nroot {:a 1 :b 2 :c {:c1 31 :c2 32} :f 5}
        z (map-zip ::root nroot)]
    (is (= [::root {:a 1 :b 2 :f 5}] (-> z down right right remove root node)))
    (is (thrown? Exception (-> z remove)))
    (is (= [::root {:a 1 :b 2 :c {} :f 5}] (-> z down right right down right remove remove root node)))
    (is (= [::root {:a 1 :b 2 :f 5}] (-> z down right right down right remove remove remove root node)))
    (is (= [::root {}] (-> (map-zip ::root {:a 1}) down remove node)))))

;; VecZipper
(deftest vector-zip-Zipper
  (let [nroot [0 1 2 [20 21] 3 [30 31 [310]]]]
    (is (= nroot (node (vector-zip nroot))))
    (is (branch? (vector-zip nroot)))
    (is (= nroot (children (vector-zip nroot))))
    (is (= nroot (make-node (vector-zip nroot) [] nroot)))
    (is (nil? (parent (vector-zip nroot))))))

(deftest vector-zip-hierarchical-navigation
  (let [grandchild [[1111] 222]
        child [grandchild 22]
        nroot [child 2]
        z (vector-zip nroot)]
    (is (= child (-> z down down parent node)))
    (is (empty? (path z)))
    (is (= child (-> z down node)))
    (is (= grandchild (-> z down down node)))
    (is (= nroot (-> z down up node)))
    (is (= nroot (-> z down down root node)))
    (is (= [nroot child] (-> z down down path)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down down down down down)))))

(deftest vector-zip-ordered-navigation
  (let [nroot [1 2 3 4]
        z (vector-zip nroot)]
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
  (let [nroot [1 2 [31 32] 4]
        z (vector-zip nroot)]
    (is (= [1 2 [30 31 32] 4] (-> z down right right down (insert-left 30) root node)))
    (is (= [1 2 [31 32 33] 4] (-> z down right right down right (insert-right 33) root node)))
    (is (thrown? Exception (-> z (insert-right 33))))
    (is (thrown? Exception (-> z (insert-left 33))))
    (is (= [1 2 3 4] (-> z down right right (replace 3) root node)))
    (is (= [0 2 [31 32] 4] (-> z down (edit dec) root node)))
    (is (= [0 1 2 [31 32] 4] (-> z (insert-child 0) root node)))
    (is (= [1] (-> (list-zip '()) (insert-child 1) node)))
    (is (= [1 2 [31 32] 4 5] (-> z (append-child 5) root node)))
    (is (= [1] (-> (list-zip []) (append-child 1) node)))))

(deftest vector-zip-iterate
  (let [nroot [1 2 [31 32] 4]
        z (vector-zip nroot)
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
  (let [nroot [1 2 [31 32] 4]
        z (vector-zip nroot)]
    (is (= [1 2 4] (-> z down right right remove root node)))
    (is (thrown? Exception (-> z remove)))
    (is (= [1 2 [] 4] (-> z down right right down right remove remove root node)))
    (is (= [1 2 4] (-> z down right right down right remove remove remove root node)))
    (is (= [] (-> (vector-zip [0]) down remove node)))))

;; index/key navigation
(deftest map-zip-key-navigation
  (let [grandchild {:a00 130 :a01 131}
        child {:a0 10 :a1 11 :a2 12 :a3 grandchild :a4 14}
        nroot {:a child :b 2}
        z (map-zip ::root nroot)]
    (is (= [:a child] (-> z (down-to :a) node)))
    (is (= [:a00 130] (-> z (down-to :a) (down-to :a3) (down-to :a00) node)))
    (is (nil? (-> z (down-to :c))))))

(deftest vector-zip-index-navigation
  (let [grandchild [130 131]
        child [10 11 12 grandchild 14]
        nroot [child 2]
        z (vector-zip nroot)]
    (is (= child (-> z (down-to 0) node)))
    (is (= 130 (-> z (down-to 0) (down-to 3) (down-to 0) node)))
    (is (nil? (-> z (down-to 9))))))

;; Edge case exploration
(deftest move-and-edit
  (let [nroot [1 1 3 [4 5]]]
    (is (= (-> nroot vector-zip down right (edit inc) root node)
           (-> nroot vector-zip down right (edit inc) right root node)))
    (is (= (-> nroot vector-zip down right right (edit inc) root node)
           (-> nroot vector-zip down right right (edit inc) left root node)))
    (is (= (-> nroot vector-zip down rightmost left (edit inc) root node)
           (-> nroot vector-zip down right right (edit inc) left root node)))))

(deftest preserve-type-on-edit
  (let [nroot (sorted-set 1 2 3)]
    (is (instance? (class nroot)
                   (-> nroot seqable-zip down right (edit dec) root node))))
  (let [nroot (list 1 2 3)]
    (is (instance? (class nroot)
                   (-> nroot list-zip down right (edit dec) root node))))
  (let [nroot (sorted-map :a 1 :b 2 :c 3)]
    (is (instance? (class nroot)
                   (-> nroot map-zip down right (edit #(update % 1 dec)) root node val))))
  (let [nroot (vector-of :long 1 2 3)]
    (is (instance? (class nroot)
                   (-> nroot vector-zip down right (edit inc) root node)))))
