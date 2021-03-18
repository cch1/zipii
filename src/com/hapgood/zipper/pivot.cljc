(ns com.hapgood.zipper.pivot
  (:require [com.hapgood.zipper :as zipper]
            [com.hapgood.zipper.loc :as loc]
            [com.hapgood.zipper.sloc :as sloc]))

(defprotocol Pivotable
  (pivot [this k] "Pivot the elements of this sequential and return a triple of [elements-before element-at elements-after"))

(extend-protocol Pivotable
  clojure.lang.MapEntry
  (pivot [[_ this] k] (when-let [me (find this k)]
                        (let [mes (vec this)
                              i (.indexOf (vec this) me)
                              [befores pivot-and-afters] (split-at i mes)]
                          [befores me (rest pivot-and-afters)])))
  clojure.lang.IPersistentVector
  (pivot [this k]
    (when-let [v (get this k)]
      [(subvec this 0 k) v (subvec this (inc k))])))

(defprotocol ChildrenByKey
  (down-to [loc cname] "Navigate to the child `loc` of this `loc` identified by the key `cname`; return nil if `cname` is not the key of any child."))

(extend-protocol ChildrenByKey
  com.hapgood.zipper.loc.Loc
  (down-to [this k] (when (zipper/branch? this)
                      (when-let [[lefts pivot rights] (pivot (zipper/tree this) k)]
                        (let [ptrees (conj (:pts this) (:t this))
                              ->treeish (:->treeish this)]
                          (loc/->Loc (->treeish pivot) [lefts (:p this) rights] ptrees ->treeish)))))
  com.hapgood.zipper.sloc.Loc
  (down-to [this k] (when (zipper/branch? this)
                      (when-let [[lefts pivot rights] (pivot (zipper/tree this) k)]
                        (let [ptrees (conj (:pts this) (:t this))
                              ->treeish (:->treeish this)]
                          (sloc/->Loc (->treeish pivot) [lefts (:p this) rights] ptrees ->treeish))))))
