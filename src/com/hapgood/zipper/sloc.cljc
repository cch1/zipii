(ns com.hapgood.zipper.sloc
  (:require [com.hapgood.zipper :as z]))

(defrecord Siblings [lefts t rights z]
  z/Treeish
  (tree [this] (first (z/z-up z (map z/tree (concat (reverse lefts) [t] rights))))))

(def ^:private top
  "A sentinel value representing the path of the tree at the top of a zipper"
  [() nil ()])

(defrecord Loc [t p z]
  z/Treeish
  (tree [this] (z/tree t))
  (branches [this] (first (z/z-dn z (z/tree this))))
  z/Zipper
  (left [this] (let [[lefts p' rights] p]
                 (when-first [l lefts] ; fails for leftmost (thus top)
                   (Loc. l [(rest lefts) p' (cons t rights)] z))))
  (right [this] (let [[lefts p' rights] p]
                  (when-first [r rights] ; fails for rightmost (thus top)
                    (Loc. r [(cons t lefts) p' (rest rights)] z))))
  (up [this] (when (not= top p)
               (let [[lefts p' rights] p]
                 (Loc. (->Siblings lefts t rights z) p' z))))
  (down [this] (if (instance? Siblings t)
                 (let [[lmts mt rmts] ((juxt :lefts :t :rights) t)]
                   (Loc. mt [lmts p rmts] z))
                 (when-let [[t' z'] (z/z-dn z t)]
                   (when-first [c t']
                     (Loc. c [() p (rest t')] z')))))
  (change [this t] (Loc. t p z))
  (insert-left [this l] (if (not= top p)
                          (let [[lefts p' rights] p
                                node [(cons l lefts) p' rights]]
                            (Loc. t node z))
                          (throw (ex-info "Can't insert left of top" {:loc this :t t}))))
  (insert-right [this r] (if (not= top p)
                           (let [[lefts p' rights] p
                                 node [lefts p' (cons r rights)]]
                             (Loc. t node z))
                           (throw (ex-info "Can't insert right of top" {:loc this :t t}))))
  (insert-down [this t1] (if (instance? Siblings t)
                           (-> this z/down (z/insert-left t1) z/left)
                           (if-let [[cs z'] (z/z-dn z t)]
                             (let [p' [() p cs]]
                               (Loc. t1 p' z'))
                             (throw (ex-info "Can only insert down from a branch" {:loc this :t t})))))
  (delete [this] (if (not= top p)
                   (let [[[l & ls :as lefts] parent [r & rs :as rights]] p]
                     (cond
                       r (Loc. r [lefts parent (sequence rs)] z)
                       l (Loc. l [(sequence ls) parent rights] z)
                       ;; Re-cast the Siblings as a mere Section
                       true (let [[t' z'] (z/z-up z ())]
                              (Loc. t' parent z'))))
                   (throw (ex-info "Can't remove at top" {:loc this :t t})))))

(defn zipper
  "Creates a new zipper structure.

  `z` is a value that satisfies com.hapgood.zipper/Zip and ensures that branch nodes can be opened and closed consistently.

  `root` is the root of the tree."
  [z root]
  (->Loc root top z))

(defn loc? [obj] (instance? Loc obj))
