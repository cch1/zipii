(ns com.hapgood.zipper.sloc
  (:require [com.hapgood.zipper :as zipper]
            [com.hapgood.zipper.loc :as loc]))

(deftype Siblings [lefts mtree rights t]
  clojure.lang.Sequential ; just a marker...
  clojure.lang.ISeq ; ...to allow sequential destructuring
  (seq [this] (list lefts mtree rights))
  zipper/TreeLike
  (tree [_] (zipper/tree t))
  (branch? [_] true)
  (branches [_] (zipper/branches t))
  (seed [_ branches] (zipper/seed t (map zipper/tree branches)))
  Object
  (equals [this other] (and (= (type this) (type other)) (= t (.-t other))))
  (hashCode [_] (.hashCode [lefts mtree rights t])))

(def ^:private top
  "A sentinel value representing the path of the tree at the top of a zipper"
  [() nil ()])

(defrecord Loc [t p pts ->treeish]
  zipper/TreeLike
  (tree [this] (zipper/tree t))
  (branch? [this] (zipper/branch? t))
  (branches [this] (zipper/branches t))
  zipper/Zipper
  (left [this] (let [[lefts p' rights] p]
                 (when-let [[l & ls] (seq lefts)] ; fails for leftmost (thus top)
                   (->Loc l [(or ls ()) p' (cons t rights)] pts ->treeish))))
  (right [this] (let [[lefts p' rights] p]
                  (when-let [[r & rs] (seq rights)] ; fails for rightmost (thus top)
                    (->Loc r [(cons t lefts) p' (or rs ())] pts ->treeish))))
  (up [this] (when (not= top p)
               (let [[lefts p' rights] p
                     t' (->Siblings lefts t rights (->treeish (zipper/seed (peek pts) (concat (reverse lefts) [t] rights))))]
                 (->Loc t' p' (pop pts) ->treeish))))
  (down [this] (when-let [[lmts mt rmts] (if (instance? Siblings t)
                                           t
                                           (when-let [[c & cs] (seq (zipper/branches t))]
                                             [() c (or cs ())]))]
                 (->Loc mt [lmts p rmts] (conj pts t) ->treeish)))
  (change [this t] (->Loc (->treeish t) p pts ->treeish))
  (insert-left [this l] (if (not= top p)
                          (let [[lefts p' rights] p
                                node [(cons (->treeish l) lefts) p' rights]]
                            (->Loc t node pts ->treeish))
                          (throw (ex-info "Can't insert left of top" {:loc this :t t}))))
  (insert-right [this r] (if (not= top p)
                           (let [[lefts p' rights] p
                                 node [lefts p' (cons (->treeish r) rights)]]
                             (->Loc t node pts ->treeish))
                           (throw (ex-info "Can't insert right of top" {:loc this :t t}))))
  (insert-down [this t1] (if (instance? Siblings t)
                           (-> this zipper/down (zipper/insert-left t1) zipper/left)
                           (if-let [cs (zipper/branches t)]
                             (let [p' [() p cs]]
                               (->Loc (->treeish t1) p' (conj pts t) ->treeish))
                             (throw (ex-info "Can only insert down from a branch" {:loc this :t t})))))
  (delete [this] (if (not= top p)
                   (let [[[l & ls :as lefts] parent [r & rs :as rights]] p]
                     (cond
                       r (->Loc r [lefts parent (or rs ())] pts ->treeish)
                       l (->Loc l [(or ls ()) parent rights] pts ->treeish)
                       ;; Re-cast the Siblings as a mere Section
                       true (->Loc (->treeish (zipper/seed (peek pts) ())) parent (pop pts) ->treeish)))
                   (throw (ex-info "Can't remove at top" {:loc this :t t})))))

(defn zipper
  "Creates a new zipper structure.

  `branches` is a fn that, given a (sub)tree, returns a possibly empty sequence of its subtrees, or nil if it is not a branch.

  `seed` is a constructor fn that, given a (sub)tree and a seq of branches, returns a new (sub)tree having the supplied child branches.

  `root` is the root of the tree."
  [branches seed root]
  (let [->treeish (loc/make->treeish branches seed)]
    (->Loc (->treeish root) top [] ->treeish)))

(defn loc? [obj] (instance? Loc obj))

(defmethod print-dup Siblings [s w] (print-ctor s (fn [s w] (print-dup (.-treeish s) w)) w))
(defmethod print-method Siblings [s w] (do (.write w "Siblings[")
                                           (print-method (.-lefts s) w)
                                           (.write w " ")
                                           (print-method (.-mtree s) w)
                                           (.write w " ")
                                           (print-method (.-rights s) w)
                                           (.write w "]")))

(defmethod print-dup Loc [l w] (print-ctor l (fn [s w] (print-dup (.-t l) w) (print-dup (.-p l) w)) w))
(defmethod print-method Loc [l w] (do (.write w "ScarLoc(")
                                      (print-method (.-t l) w)
                                      (.write w ", ")
                                      (print-method (.-p l) w)
                                      (.write w ")")))
