(ns com.hapgood.zipper.loc
  (:require [com.hapgood.zipper :as zipper]))

;; Issues with the OCaml psuedo-implementation by Huet:
;; 1. Insertion is inconsistent with respect to the resulting position: at inserted element for some (`insert-down`) but in the original position (`insert-left`, `insert-right`) for others.  Did Hickey choose a different name (`insert-child`, with no movement, for `insert-down`) to be consistent without clashing with the original implementation?  In any case, I have preserved the original behavior here.
;; 2. The implementation does not provide much detail on error conditions/failures and their translation in Clojure.  Here I have leveraged Clojure's `ex-info` to provide clear feedback for exceptions.  I have also adopted Hickey's approach of returning falsey for some failures that are not considered exceptions.
;; 3. Without the complexity of scars, performance is likely to suffer when movement is more vertical.

(deftype Section [trees t seed]
  zipper/TreeLike
  (tree [this] t)
  (branch? [_] true)
  (branches [_] trees) ; return the cached value
  (seed [_ bs] (let [t (seed t (map zipper/tree bs))]
                 (Section. bs t seed)))
  Object
  (equals [this other] (and (= (type this) (type other))
                            (= [t seed] [(.-t other) (.-seed other)])))
  (hashCode [this] (.hashCode [trees t seed])))

(def top
  "A sentinel value representing the path of the tree at the top of a zipper"
  [() nil ()])

(defrecord Loc [t p pts ->treeish]
  zipper/TreeLike
  (tree [this] (zipper/tree t))
  (branch? [this] (zipper/branch? t))
  (branches [this] (zipper/branches t))
  zipper/Zipper
  (left [this] (let [[lefts up rights] p]
                 (when-let [[l & ls] (seq lefts)] ; fails for leftmost (thus top)
                   (->Loc l [(or ls ()) up (cons t rights)] pts ->treeish))))
  (right [this] (let [[lefts up rights] p]
                  (when-let [[r & rs] (seq rights)] ; fails for rightmost (thus top)
                    (->Loc r [(cons t lefts) up (or rs ())] pts ->treeish))))
  (up [this] (when (not= top p)
               (let [[lefts up rights] p
                     t (zipper/seed (peek pts) (concat (reverse lefts) (cons t rights)))]
                 (->Loc t up (pop pts) ->treeish))))
  (down [this] (when-let [[t1 & trees] (seq (zipper/branches t))]
                 (->Loc t1 [() p (or trees ())] (conj pts t) ->treeish)))
  (change [this t'] (->Loc (->treeish t') p pts ->treeish))
  (insert-left [this l] (if (not= top p)
                          (let [[lefts up rights] p
                                node [(cons (->treeish l) lefts) up rights]]
                            (->Loc t node pts ->treeish))
                          (throw (ex-info "Can't insert left of top" {:loc this :t t}))))
  (insert-right [this r] (if (not= top p)
                           (let [[lefts up rights] p
                                 node [lefts up (cons (->treeish r) rights)]]
                             (->Loc t node pts ->treeish))
                           (throw (ex-info "Can't insert right of top" {:loc this :t t}))))
  (insert-down [this t1] (if-let [sons (zipper/branches t)]
                           (let [node [() p sons]]
                             (->Loc (->treeish t1) node (conj pts t) ->treeish))
                           (throw (ex-info "Can only insert down from a branch" {:loc this :t t}))))
  (delete [this] (if (not= top p)
                   (let [[[l & ls :as lefts] up [r & rs :as rights]] p]
                     (cond
                       r (->Loc r [lefts up (or rs ())] pts ->treeish)
                       l (->Loc l [(or ls ()) up rights] pts ->treeish)
                       true (->Loc (zipper/seed (peek pts) ()) up (pop pts) ->treeish)))
                   (throw (ex-info "Can't remove at top" {:loc this :t t})))))

(defn make->treeish [branches* seed*] (fn ->treeish [t] (if-let [branches (branches* t)]
                                                          (->Section (map ->treeish branches) t seed*)
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

(defmethod print-dup Section [s w] (print-ctor s (fn [s w] (print-dup (.-treeish s) w)) w))
(defmethod print-method Section [s w] (do (.write w "Section[")
                                          (print-method (.-trees s) w)
                                          (.write w "]")))

(defmethod print-dup Loc [l w] (print-ctor l (fn [s w] (print-dup (.-t l) w) (print-dup (.-p l) w)) w))
(defmethod print-method Loc [l w] (do (.write w "Loc(")
                                      (print-method (.-t l) w)
                                      (.write w ", ")
                                      (print-method (.-p l) w)
                                      (.write w ")")))
