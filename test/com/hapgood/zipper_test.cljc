(ns com.hapgood.zipper-test
  (:require [com.hapgood.zipper :as zipper :refer [nth-child tree]]
            [com.hapgood.zipper.loc :as loc]
            [clojure.test :as test :refer [deftest is]]))

(defrecord ListZip []
  zipper/Zip
  (z-dn [this t] (when (list? t) [t this] ))
  (z-up [this branches] [(apply list branches) this]))

(def list-zip (partial loc/zipper (->ListZip)))

(deftest nth-child-porcelain
  (is (= 4 (-> (apply list (range 100)) list-zip (nth-child 5) tree))))
