(ns com.hapgood.zipper.pivot-test
  (:refer-clojure :exclude [replace next remove])
  (:require [com.hapgood.zipii :refer [vector-zip map-zip node]]
            [com.hapgood.zipper.pivot :refer [down-to]]
            [clojure.test :refer [deftest is are]]))

(deftest zip-index-navigation
  (let [grandchild [130 131]
        child [10 11 12 grandchild 14]
        t [child 2]
        z (vector-zip t)]
    (is (= child (-> z (down-to 0) node)))
    (is (= 130 (-> z (down-to 0) (down-to 3) (down-to 0) node)))
    (is (nil? (-> z (down-to 9))))))

(deftest map-zip-key-navigation
  (let [grandchild {:a00 130 :a01 131}
        child {:a0 10 :a1 11 :a2 12 :a3 grandchild :a4 14}
        t {:a child :b 2}
        z (map-zip ::root t)]
    (is (= [:a child] (-> z (down-to :a) node)))
    (is (= [:a00 130] (-> z (down-to :a) (down-to :a3) (down-to :a00) node)))
    (is (nil? (-> z (down-to :c))))))
