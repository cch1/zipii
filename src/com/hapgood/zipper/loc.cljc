(ns com.hapgood.zipper.loc
  (:require [com.hapgood.zipper :as zipper]))

;; Issues with the OCaml psuedo-implementation by Huet:
;; 1. Insertion is inconsistent with respect to the resulting position: at inserted element for some (`insert-down`) but in the original position (`insert-left`, `insert-right`) for others.  Did Hickey choose a different name (`insert-child`, with no movement, for `insert-down`) to be consistent without clashing with the original implementation?  In any case, I have preserved the original behavior here.
;; 2. The implementation does not provide much detail on error conditions/failures and their translation in Clojure.  Here I have leveraged Clojure's `ex-info` to provide clear feedback for exceptions.  I have also adopted Hickey's approach of returning falsey for some failures that are not considered exceptions.
;; 3. Without the complexity of scars, performance is likely to suffer when movement is more vertical.

(def top
  "A sentinel value representing the path of the tree at the top of a zipper"
  [() nil ()])

(defrecord Loc [t p z]
  zipper/Treeish
  (tree [this] t)
  (branches [this] (first (zipper/z-dn z (zipper/tree this))))
  zipper/Zipper
  (left [this] (let [[lefts up rights] p]
                 (when-let [[l & ls] (seq lefts)] ; fails for leftmost (thus top)
                   (->Loc l [(sequence ls) up (cons t rights)] z))))
  (right [this] (let [[lefts up rights] p]
                  (when-let [[r & rs] (seq rights)] ; fails for rightmost (thus top)
                    (->Loc r [(cons t lefts) up (sequence rs)] z))))
  (up [this] (when (not= top p)
               (let [[lefts up rights] p
                     [t z] (zipper/z-up z (concat (reverse lefts) (cons t rights)))]
                 (->Loc t up z))))
  (down [this] (when-let [[trees z] (zipper/z-dn z t)]
                 (when-let [[t1 & trees] (seq trees)]
                   (->Loc t1 [() p (sequence trees)] z))))
  (change [this t'] (->Loc t' p z))
  (insert-left [this l] (if (not= top p)
                          (let [[lefts up rights] p
                                node [(cons l lefts) up rights]]
                            (->Loc t node z))
                          (throw (ex-info "Can't insert left of top" {:loc this :t t}))))
  (insert-right [this r] (if (not= top p)
                           (let [[lefts up rights] p
                                 node [lefts up (cons r rights)]]
                             (->Loc t node z))
                           (throw (ex-info "Can't insert right of top" {:loc this :t t}))))
  (insert-down [this t1] (if-let [[sons z] (zipper/z-dn z t)]
                           (let [node [() p sons]]
                             (->Loc t1 node z))
                           (throw (ex-info "Can only insert down from a branch" {:loc this :t t}))))
  (delete [this] (if (not= top p)
                   (let [[[l & ls :as lefts] up [r & rs :as rights]] p]
                     (cond
                       r (->Loc r [lefts up (sequence rs)] z)
                       l (->Loc l [(sequence ls) up rights] z)
                       true (let [[t z] (zipper/z-up z ())] (->Loc t up z))))
                   (throw (ex-info "Can't remove at top" {:loc this :t t})))))

(defn zipper
  "Creates a new zipper structure.

  `z` is a value that satisfies com.hapgood.zipper/Zip and ensures that branch nodes can be opened and closed consistently.

  `root` is the root of the tree."
  [z root]
  (->Loc root top z))

(defn loc? [obj] (instance? Loc obj))
