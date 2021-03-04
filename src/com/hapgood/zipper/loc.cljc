(ns com.hapgood.zipper.loc
  (:require [com.hapgood.zipper :as zipper]))

(deftype Section [trees treeish branches seed]
  clojure.lang.Indexed
  (nth [this n] (nth trees n))
  zipper/TreeLike
  (tree [this] (seed treeish (map zipper/tree trees)))
  (branch? [_] true)
  (branches [_] trees)
  (seed [_ bs] (let [t (seed treeish (map zipper/tree bs))] (Section. (branches t) t branches seed)))
  Object
  (equals [this other] (and (= (type this) (type other))
                            (= [trees treeish branches seed]
                               [(.-trees other) (.-treeish other) (.-branches other) (.-seed other)])))
  (hashCode [this] (.hashCode [trees treeish branches seed])))

(defmethod print-method Section [s w] (.write w "<") (.write w (str (zipper/branches s))) (.write w ">"))

(def top
  "A sentinel value representing the path of the tree at the top of a zipper"
  [() nil ()])

(defrecord Loc [t p pts ->treeish]
  zipper/TreeLike
  (tree [this] (zipper/tree t))
  (branch? [this] (zipper/branch? t))
  (branches [this] (zipper/branches t))
  zipper/Zipper
  (left [this] (when (not= top p)
                 (let [[lefts parent rights] p]
                   (when-let [[l & ls] (seq lefts)]
                     (->Loc (->treeish l) [(or ls ()) parent (cons t rights)] pts ->treeish)))))
  (right [this] (when (not= top p)
                  (let [[lefts parent rights] p]
                    (when-let [[r & rs] (seq rights)]
                      (->Loc (->treeish r) [(cons t lefts) parent (or rs ())] pts ->treeish)))))
  (up [this] (when (not= top p)
               (let [[lefts parent rights] p
                     t (zipper/seed (peek pts) (concat (reverse lefts) (cons t rights)))]
                 (->Loc t parent (pop pts) ->treeish))))
  (down [this] (when (zipper/branch? t)
                 (when-let [[t1 & trees] (seq (zipper/branches t))]
                   (->Loc (->treeish t1) [() p (or trees ())] (conj pts t) ->treeish))))
  (change [this t'] (->Loc (->treeish t') p pts ->treeish))
  (insert-left [this l] (if (not= top p)
                          (let [[lefts parent rights] p
                                node [(cons (->treeish l) lefts) parent rights]]
                            (->Loc t node pts ->treeish))
                          (throw (ex-info "Can't insert left of top" {:loc this :t t}))))
  (insert-right [this r] (if (not= top p)
                           (let [[lefts parent rights] p
                                 node [lefts parent (cons (->treeish r) rights)]]
                             (->Loc t node pts ->treeish))
                           (throw (ex-info "Can't insert right of top" {:loc this :t t}))))
  (insert-down [this t1] (if (zipper/branch? t)
                           (let [node [() p (zipper/branches t)]]
                             (->Loc (->treeish t1) node (conj pts t) ->treeish))
                           (throw (ex-info "Can only insert down from a branch" {:loc this :t t}))))
  (delete [this] (if (not= top p)
                   (let [[[l & ls :as lefts] parent [r & rs :as rights]] p]
                     (cond
                       r (->Loc r [lefts parent (or rs ())] pts ->treeish)
                       l (->Loc l [(or ls ()) parent rights] pts ->treeish)
                       true (->Loc (zipper/seed (peek pts) ()) parent (pop pts) ->treeish)))
                   (throw (ex-info "Can't remove at top" {:loc this :t t})))))

(defn make->treeish [branches* seed*] (fn [t] (if-let [branches (branches* t)]
                                                (->Section branches t branches* seed*)
                                                t)))

(defn zipper
  "Creates a new zipper structure.

  `branches` is a fn that, given a (sub)tree, returns a possibly empty sequence of its subtrees, or nil if it is not a branch.

  `seed` is a constructor fn that, given a (sub)tree and a seq of branches, returns a new (sub)tree having the supplied child branches.

  `root` is the root of the tree."
  [branches seed root]
  (let [->treeish (make->treeish branches seed)]
    (->Loc (->treeish root) top [] ->treeish)))

(defn loc? [obj] (instance? Loc obj))
