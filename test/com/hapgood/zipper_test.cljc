(ns com.hapgood.zipper-test
  (:require [com.hapgood.zipper :refer :all]
            [com.hapgood.zipper.loc :as loc]
            [com.hapgood.zipper.section :as section]
            [clojure.test :refer [deftest is are]]))

(def list-zip
  "Return a zipper for nested lists, given a root list"
  (partial loc/zipper (section/make->treeish (fn [tree] (when (list? tree) tree)) (comp reverse (fn [tree children] (into (empty tree) children))))))

(deftest nth-child-porcelain
  (is (= 4 (-> (apply list (range 100)) list-zip (nth-child 5) tree))))
