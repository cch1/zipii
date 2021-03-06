(ns com.hapgood.zipper.pivot
  (:require [com.hapgood.zipper :as z]
            [com.hapgood.zipper.loc :as loc]
            [com.hapgood.zipper.sloc :as sloc]))

(defprotocol Pivotable
  (pivot [this k] "Pivot the elements of this sequential and return a triple of [elements-before element-at elements-after"))

(extend-protocol Pivotable
  ;; the semantics are dubious - we are really pivoting the value (presumably a map!) of this MapEntry.
  #?(:clj clojure.lang.MapEntry :cljs cljs.core/MapEntry)
  (pivot [[_ m] k] (when-let [me (find m k)]
                     (let [mes (vec m)
                           [befores [pivot & afters]] (split-at (.indexOf mes me) mes)]
                       [befores pivot afters])))
  #?(:clj clojure.lang.IPersistentVector :cljs cljs.core/PersistentVector)
  (pivot [this k]
    (when-let [v (get this k)]
      [(subvec this 0 k) v (subvec this (inc k))])))

(defprotocol ChildrenByKey
  (down-to [loc k] "Navigate to the child `loc` of this `loc` identified by the key `k`; return nil if `k` is not the key of any child."))

(extend-protocol ChildrenByKey
  com.hapgood.zipper.loc.Loc
  (down-to [this k] (when-let [[[lefts pivot rights] z] (z/z-dn (:z this) (:t this) k)]
                      (loc/->Loc pivot [lefts (:p this) rights] z)))
  com.hapgood.zipper.sloc.Loc
  (down-to [this k] (when-let [[[lefts pivot rights] z] (z/z-dn (:z this) (:t this) k)]
                      (sloc/->Loc pivot [lefts (:p this) rights] z))))
