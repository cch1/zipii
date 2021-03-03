(ns com.hapgood.zipper.loc-test
  (:require [com.hapgood.zipper.loc :refer :all]
            [com.hapgood.zipper :refer [up down left right insert-left insert-right insert-down change delete nth-child
                                        tree branch? branches]]
            [clojure.test :refer [deftest is are]])
  (:import (clojure.lang ExceptionInfo)))

(def list-zip
  "Return a zipper for nested lists, given a root list"
  (partial zipper list? identity (comp reverse (fn [tree children] (into (empty tree) children)))))

(deftest access-move-query
  (let [t '(1 (21 22) 3)
        z (list-zip t)]
    (is (= t (-> z tree)))
    (is (= t (-> z branches)))
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
    (is (empty? (-> z branches)))
    (is (nil? (-> z left)))
    (is (nil? (-> z right)))
    (is (nil? (-> z up)))
    (is (nil? (-> z down))))
  (let [t '(1) z (list-zip t)] ; singleton
    (is (not (empty? (-> z branches))))
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

(deftest nth-child-porcelain
  (is (= 4 (-> (apply list (range 100)) list-zip (nth-child 5) tree))))

;; Edge case exploration
(deftest move-and-edit
  (let [t '(1 1 3 (4 5))]
    (is (= (-> t list-zip down right (change 0) up tree)
           (-> t list-zip down right (change 0) right up tree)))
    (is (= (-> t list-zip down right right (change 0) up tree)
           (-> t list-zip down right right (change 0) left up tree)))))
