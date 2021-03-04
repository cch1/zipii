(ns com.hapgood.zipper.sloc
  (:require [com.hapgood.zipper :as zipper]
            [com.hapgood.zipper.loc :as loc]))

;;; ScarLoc
(deftype Siblings [lefts mtree rights treeish]
  clojure.lang.Indexed ; to allow basic sequential destructuring
  (nth [this n] (vector (nth [lefts mtree rights] n)))
  (nth [this n default] (nth [lefts mtree rights] n default))
  zipper/TreeLike
  (tree [_] (zipper/tree (zipper/seed treeish (map zipper/tree (concat (reverse lefts) [mtree] rights)))))
  (branch? [_] true)
  (branches [_] (concat lefts [mtree] rights))
  (seed [_ ls mt rs] (Siblings. ls mt rs treeish))
  (seed [_ branches] (zipper/seed treeish branches)))

(defmethod print-method Siblings [s w] (let [[l m r] s] (.write w "-<") (.write w (str (vector l m r))) (.write w ">-")))

(defn- section->siblings [section item] (->Siblings () item (zipper/branches section) section))

(def ^:private top
  "A sentinel value representing the path of the tree at the top of a zipper"
  [() nil ()])

(defrecord SLoc [t p ptrees ->treeish]
  zipper/TreeLike
  (tree [this] (zipper/tree t))
  (branch? [this] (zipper/branch? t))
  (branches [this] (zipper/branches t))
  zipper/Zipper
  (left [this] (when (not= top p)
                 (let [[[l & ls :as lefts] p' rights] p
                       node [(or ls ()) p' (cons t rights)]]
                   (when l (->SLoc (->treeish l) node ptrees ->treeish)))))
  (right [this] (when (not= top p)
                  (let [[lefts p' [r & rs :as rights]] p
                        node [(cons t lefts) p' (or rs ())]]
                    ;; DANGER
                    (when r (->SLoc (->treeish r) node ptrees ->treeish)))))
  (up [this] (when (not= top p)
               (let [[lefts p' rights] p]
                 (->SLoc (zipper/seed (peek ptrees) lefts t rights) p' (pop ptrees) ->treeish))))
  (down [this] (when-let [branches (zipper/branches t)]
                 (when (seq branches)
                   (let [[lmts mt rmts :as s] t]
                     (->SLoc (->treeish mt) [lmts p rmts] (conj ptrees t) ->treeish)))))
  (change [this t] (->SLoc (->treeish t) p ptrees ->treeish))
  (insert-left [this l] (if (not= top p)
                          (let [[lefts p' rights] p
                                node [(cons (->treeish l) lefts) p' rights]]
                            (->SLoc t node ptrees ->treeish))
                          (throw (ex-info "Can't insert left of top" {:loc this :t t}))))
  (insert-right [this r] (if (not= top p)
                           (let [[lefts p' rights] p
                                 node [lefts p' (cons (->treeish r) rights)]]
                             (->SLoc t node ptrees ->treeish))
                           (throw (ex-info "Can't insert right of top" {:loc this :t t}))))
  (insert-down [this t1] (if-let [branches (zipper/branches t)] ; branch?
                           (let [[rights t'] (if-let [branches (seq branches)] ; not empty?/memo tree instead of section?
                                               [branches t]
                                               [() (section->siblings t t1)])
                                 node [() p rights]]
                             (->SLoc (->treeish t1) node (conj ptrees t') ->treeish))
                           (throw (ex-info "Can only insert down from a branch" {:loc this :t t}))))
  (delete [this] (if (not= top p)
                   (let [[[l & ls :as lefts] parent [r & rs :as rights]] p]
                     (cond
                       r (->SLoc r [lefts parent (or rs ())] ptrees ->treeish)
                       l (->SLoc l [(or ls ()) parent rights] ptrees ->treeish)
                       true (->SLoc (zipper/seed (peek ptrees) ()) parent (pop ptrees) ->treeish)))
                   (throw (ex-info "Can't remove at top" {:loc this :t t})))))


;; TODO: unify branch? and branches
(defn- make->treeish [branches* seed*] (comp (fn [t] (if-let [[c & cs] (seq (zipper/branches t))]
                                                       (->Siblings () c (or cs ()) t)
                                                       t))
                                             (loc/make->treeish branches* seed*)))

(defn zipper
  "Creates a new zipper structure.

  `branches` is a fn that, given a (sub)tree, returns a possibly empty sequence of its subtrees, or nil if it is not a branch.

  `seed` is a constructor fn that, given a (sub)tree and a seq of branches, returns a new (sub)tree having the supplied child branches.

  `root` is the root of the tree."
  [branches seed root]
  (let [->treeish (make->treeish branches seed)]
    (->SLoc (->treeish root) top [] ->treeish)))

(defn loc? [obj] (instance? SLoc obj))
