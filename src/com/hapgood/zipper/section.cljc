(ns com.hapgood.zipper.section
  (:require [com.hapgood.zipper :as zipper]))

(deftype Section [trees t seed]
  zipper/TreeLike
  (tree [this] t)
  (branch? [_] true)
  (branches [_] trees) ; return the cached value
  (seed [_ bs] (seed t (map zipper/tree bs)))
  Object
  (equals [this other] (and (= (type this) (type other))
                            (= [t seed] [(.-t other) (.-seed other)])))
  (hashCode [this] (.hashCode [trees t seed])))

(defn make->treeish [branches* seed*] (fn ->treeish [t] (if-let [branches (branches* t)]
                                                          (->Section (map ->treeish branches) t seed*)
                                                          t)))

(defmethod print-dup Section [s w] (print-ctor s (fn [s w] (print-dup (.-treeish s) w)) w))

(defmethod print-method Section [s w] (do (.write w "Section[")
                                          (print-method (.-trees s) w)
                                          (.write w "]")))
