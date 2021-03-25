(ns com.hapgood.zipper.sloc-test
  (:require [com.hapgood.zipper.sloc :refer :all]
            [com.hapgood.zipper :as zipper :refer [up down left right insert-left insert-right insert-down change delete nth-child tree]]
            [clojure.test :refer [deftest is are]])
  (:import (clojure.lang ExceptionInfo)))


(defrecord ListZip []
  zipper/Zip
  (z-dn [this t] (when (list? t) [t this] ))
  (z-up [this branches] [(apply list branches) this]))

(def list-zip (partial zipper (->ListZip)))

(deftest access-move-query
  (let [t '(1 (21 22) 3)
        z (list-zip t)]
    (is (= t (-> z tree)))
    (is (= 1 (-> z down tree)))
    (is (= '(21 22) (-> z down right tree)))
    (is (= 1 (-> z down right left tree)))
    (is (= t (-> z down up tree)))))

(deftest mutate
  (let [t '(1 (21) 3)
        z (list-zip t)]
    (let [z (-> z down right (change 2))]
      (is (= 2 (-> z tree)))
      (is (= '(1 2 3) (-> z up tree))))
    (let [z (-> z down (insert-left 0))]
      (is (= 1 (-> z tree)))
      (is (= '(0 1 (21) 3) (-> z up tree))))
    (let [z (-> z down (insert-right 3/2))]
      (is (= 1 (-> z tree)))
      (is (= '(1 3/2 (21) 3) (-> z up tree))))
    (let [z (-> z (insert-down 0))]
      (is (= 0 (-> z tree)))
      (is (= '(0 1 (21) 3) (-> z up tree))))
    (let [z (-> z down right delete)]
      (is (= 3 (-> z tree)))
      (is (= '(1 3) (-> z up tree))))
    (let [z (-> z down right right delete)]
      (is (= '(21) (-> z tree)))
      (is (= '(1 (21)) (-> z up tree))))
    (let [z (-> z down right down delete)]
      (is (= () (-> z tree)))
      (is (= '(1 () 3) (-> z up tree))))))

;; Test invariants
(defn pair [x f] [x (f x)])

(deftest invariant-access-move-query
  (let [t nil z (list-zip t)] ; degenerate
    (is (= t (-> z tree)))
    (is (nil? (-> z left)))
    (is (nil? (-> z right)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down))))
  (let [t () z (list-zip t)] ; empty
    (is (nil? (-> z left)))
    (is (nil? (-> z right)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down))))
  (let [t '(1) z (list-zip t)] ; singleton
    (is (loc? (-> z down)))))

(deftest invariant-mutate
  (let [t nil z (list-zip t)]
    (is (thrown? ExceptionInfo (-> z delete)))
    (is (thrown? ExceptionInfo (-> z (insert-right nil))))
    (is (thrown? ExceptionInfo (-> z (insert-left nil))))
    (is (thrown? ExceptionInfo (-> z (insert-down nil)))))
  (let [t () z (list-zip t)]
    (is (thrown? ExceptionInfo (-> z delete)))
    (is (thrown? ExceptionInfo (-> z (insert-right nil))))
    (is (thrown? ExceptionInfo (-> z (insert-left nil))))
    (is (loc? (-> z (insert-down [:a 1]) up))))
  (let [t '(1) z (list-zip t)]
    (is (= t (let [z (-> z down)
                   item (-> z tree)]
               (-> z delete (insert-down item) up tree))))))

(deftest return-down-via-scar
  (is (= 3 (-> '(1 2 3 4) list-zip down right right up down tree))))

(deftest insert-down-via-scar
  (is (= '(0 1 2 3 4 5)
         (-> '(0 1 4) list-zip down right right up (insert-down 2) right (insert-right 5) (insert-left 3) up tree))))

;; Edge case exploration
(deftest move-and-edit
  (let [t '(1 1 3 (4 5))]
    (is (= (-> t list-zip down right (change 0) up tree)
           (-> t list-zip down right (change 0) right up tree)))
    (is (= (-> t list-zip down right right (change 0) up tree)
           (-> t list-zip down right right (change 0) left up tree)))))

(deftest nil-elements
  (let [t '(nil nil nil '(nil nil))
        z (list-zip t)]
    (is (= t (-> z down right left up tree)))))

(deftest spontaneous-generation
  (let [t 1]
    (is (= '(1) (-> t list-zip (change ()) (insert-down 1) up tree)))))

(defrecord SeqZip []
  zipper/Zip
  (z-dn [this t] (when (seq? t) [t this] ))
  (z-up [this branches] [branches this]))

(def seq-zip (partial zipper (->SeqZip)))

(deftest infinitely-deep-tree
  (is (let [t ((fn lazy-tree [] (lazy-seq (list (lazy-tree)))))
            fut (future (do (-> t seq-zip down down down up up up tree) true))]
        (try (deref fut 500 nil)
             (finally (future-cancel fut))))))

(deftest infinitely-wide-tree
  (is (let [t (range)
            fut (future (do (-> t seq-zip down right left up tree) true))]
        (try (deref fut 500 nil)
             (finally (future-cancel fut))))))

(deftest serialize
  (let [z (-> '(1 (21 22) 3) list-zip down right down right up)]
    (is (= 22 (-> (read-string (pr-str z)) down tree)))))
