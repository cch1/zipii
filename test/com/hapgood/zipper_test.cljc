(ns com.hapgood.zipper-test
  (:require [com.hapgood.zipper :refer :all]
            [clojure.zip :as z]
            [clojure.test :refer [deftest is]])
  (:refer-clojure :exclude (replace remove next)))

(deftest seq-zip-Zipper
  (let [nroot '(0 1 2 (20 21) 3 (30 31 (310)))]
    (is (= nroot (node (seq-zip nroot))))
    (is (branch? (seq-zip nroot)))
    (is (= nroot (children (seq-zip nroot))))
    (is (= nroot (make-node (seq-zip nroot) [] nroot)))))

(deftest seq-zip-hierarchical-navigation
  (let [grandchild '((1111) 222)
        child (list grandchild 22)
        nroot (list child 2)
        z (seq-zip nroot)]
    (is (root? z))
    (is (empty? (path z)))
    (is (= child (-> z down node)))
    (is (= grandchild (-> z down down node)))
    (is (= nroot (-> z down up node)))
    (is (= nroot (-> z down down root node)))
    (is (= [nroot child] (-> z down down path)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down down down down down)))))

(deftest seq-zip-ordered-navigation
  (let [nroot (list 1 2 3 4)
        z (seq-zip nroot)]
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

(deftest seq-zip-update
  (let [nroot '(1 2 (31 32) 4)
        z (seq-zip nroot)]
    (is (= '(1 2 (30 31 32) 4) (-> z down right right down (insert-left 30) root node)))
    (is (= '(1 2 (31 32 33) 4) (-> z down right right down right (insert-right 33) root node)))
    (is (= '(1 2 3 4) (-> z down right right (replace 3) root node)))
    (is (= '(0 2 (31 32) 4) (-> z down (edit dec) root node)))
    (is (= '(0 1 2 (31 32) 4) (-> z (insert-child 0) root node)))
    (is (= '(1) (-> (seq-zip '()) (insert-child 1) node)))
    (is (= '(1 2 (31 32) 4 5) (-> z (append-child 5) root node)))
    (is (= '(1) (-> (seq-zip '()) (append-child 1) node)))))

(deftest seq-zip-iterate
  (let [nroot '(1 2 (31 32) 4)
        z (seq-zip nroot)]
    (is (= 1 (-> z next node)))
    (is (= '(31 32) (-> z next next next node)))
    (is (= 31 (-> z next next next next node)))
    (is (= 2 (-> z next next next next prev prev node)))))

(deftest seq-zip-remove
  (let [nroot '(1 2 (31 32) 4)
        z (seq-zip nroot)]
    (is (= '(1 2 4) (-> z down right right remove root node)))
    (is (= '(1 2 () 4) (-> z down right right down right remove remove root node)))
    (is (= `(1 2 4) (-> z down right right down right remove remove remove root node)))
    ;; This next one exposes a bug in clojure.zip...
    (is (= () (-> (seq-zip '(0)) down remove node)))))
