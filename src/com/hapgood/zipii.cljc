(ns com.hapgood.zipii
  "Functions for navigating and editing of tree-like data structures using a zipper
  This namespace attempts to duplicate the functionality of clojure.zip."
  (:refer-clojure :exclude (replace remove next))
  (:require [com.hapgood.zipper.loc :as loc]
            [com.hapgood.zipper.pivot :as pivot]
            [com.hapgood.zipper :as z]))

;; Reference: https://www.st.cs.uni-saarland.de//edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf

;; Issues with implementation by Hickey:
;; 1. The use of metadata "seems" less idiomatic than a protocol & defrecord.  Performance may or may not suffer (TBD) but the syntax with protocols more closely matches the intent of the code, in my opinion.
;; 2. Sentinel values like `:end` "seem" less idiomatic than namespaced keywords.
;; 4. There is a bug in the original implementation when deleting the last child of a node in a seq-zip.
;; 5. The `root` function complects `move` and `return-updated-data-structure`.  In fact, there is no (simple) way to move to the root without also exiting the zipper.  This is surprising and it's not too hard to string together two simple functions (`root` followed by `node`) to accomplish the combined effect.
;; 6. The `changed?` attribute increases performance relative to the base Huet implementation (in which the tree is reconstituted when zipping up regardless of changes) when movement is more vertical, but at the expense of complexity.  I will explore scars for this purpose at some later time.
;; 7. The vocabulary diverges from Huet and while Hickey's seems more reasonable, the Huet paper is an excellent implementation guide and sticking to his vocabulary reinforces that effect.
;; 8. Exceptions are not very precise nor are they data-laden.
;; 9. Zippers are not serializable.

(def left z/left)
(def right z/right)
(def up z/up)
(def down z/down)
(def change z/change)
(def insert-left z/insert-left)
(def insert-right z/insert-right)
(def insert-down z/insert-down)
(def delete z/delete)

(def nth-child z/nth-child)

;; Iterative navigation
(defn- make-end
  "Create a sentinel loc that can only result from navigating beyond the limits of the data structure"
  [loc]
  (let [throw! (fn [](throw (ex-info "Operation not allowed on end loc" {::root loc})))]
    ^{::end loc} (reify
                   z/Treeish
                   (tree [loc] (throw!))
                   (branches [loc] (throw!))
                   z/Zipper
                   (left [loc] (throw!))
                   (right [loc] (throw!))
                   (up [loc] (throw!))
                   (down [loc] (throw!))
                   (change [loc t] (throw!))
                   (insert-left [loc t] (throw!))
                   (insert-right [loc t] (throw!))
                   (insert-down [loc t] (throw!))
                   (delete [loc] (throw!)))))

(def end?
  "Return wormholed loc if loc represents the end of a depth-first walk, otherwise nil"
  (comp ::end meta))

(defn root
  "Zip all the way up and return the root, reflecting any changes."
  [loc]
  (z/tree (or (end? loc)
              (loop [loc loc]
                (if-let [loc' (up loc)] (recur loc') loc)))))

(defn path
  "Return a seq of trees leading to this loc"
  [loc]
  (if-let [ploc (up loc)]
    (conj (path ploc) (z/tree ploc))
    []))

(defn rightmost
  "Returns the loc of the leftmost sibling of the tree at this loc, or self"
  [loc]
  (loop [loc loc]
    (if-let [r (right loc)]
      (recur r)
      loc)))

(defn leftmost
  "Returns the loc of the leftmost sibling of the tree at this loc, or self"
  [loc]
  (loop [loc loc]
    (if-let [r (left loc)]
      (recur r)
      loc)))

(defn next
  "Moves to the next loc in the hierarchy, depth-first. When reaching the end, returns
  a distinguished loc detectable via end?.  If already at the end, stays there."
  [loc]
  (if (end? loc)
    loc
    (or
     (down loc)
     (right loc)
     (loop [p loc]
       (if-let [p' (up p)]
         (or (right p') (recur p'))
         (make-end p))))))

(defn prev
  "Moves to the previous loc in the hierarchy, depth-first. If already at the root, returns nil."
  [loc]
  (if-let [l (left loc)]
    (loop [loc l]
      (if-let [child (down loc)]
        (recur (rightmost child))
        loc))
    (up loc)))

(defn edit
  "Replace the tree at this loc with the value of (f tree args)"
  [loc f & args]
  (change loc (apply f (z/tree loc) args)))

(defn append-down
  "Inserts the item as the rightmost child of the tree at this loc"
  [loc tree]
  (if-let [oldest (down loc)]
    (let [youngest (rightmost oldest)]
      (right (insert-right youngest tree)))
    (insert-down loc tree)))

(defn- iteratively
  "Return a function that composes n applications of f"
  [n f]
  (fn [x] (loop [i n x x]
            (if (zero? i) x (recur (dec i) (f x))))))

(defn depth [loc]
  (when loc (loop [[_ p _] (:p loc) i 0]
              (if p (recur p (inc i)) i))))

(defn- slew
  "Slew the first loc to the DFS position of the second, presuming loc1's DFS predecessors have not been changed in loc0's tree"
  ;; When loc0 and loc1 have the same parent and common elder siblings they are, by this definition, in the same position.
  ;; NB: this condition will not hold if either loc has zip-propogated independent changes up to a shared ancestor.
  ;; The intent of this operation is to "pull" loc0 to a DFS predecessor in its tree, dragging along any zipped changes loc0 may embody.
  ;; To prevent independent changes from being zipped into a parent (and changing their identity), the strategy is to move both locs up
  ;; until they have identical parents and then move loc0 back through the recorded path followed by loc1 to arrive at the common ancestor.
  [loc0 loc1]
  (loop [loc0 loc0 loc1 loc1 s identity]
    (let [[lefts0 parent0 _]  (:p loc0)
          [lefts1 parent1 _]  (:p loc1)
          ls1 (count lefts1)]
      (if (= parent0 parent1)
        (if (= lefts0 lefts1)
          (s loc0)  ; replay on loc0 how loc1 got here
          (recur (leftmost loc0) (leftmost loc1) (comp s (iteratively ls1 right))))
        (let [ds0 (depth loc0)
              ds1 (depth loc1)
              delta-d (- ds1 ds0)]
          (cond
            (pos? delta-d) (recur loc0 (up loc1) (comp s (iteratively ls1 right) down))
            (neg? delta-d) (recur (up loc0) loc1 s)
            true (recur (up loc0) (up loc1) (comp s (iteratively ls1 right) down))))))))

(def replace "Replaces the node at this loc, without moving" change)
(defn remove
  "Remove the node at loc, returning the loc that would have preceded it in a depth-first walk."
  [loc]
  (slew (delete loc) (prev loc)))
(def insert-child "Insert the item as the leftmost child of the node at this loc, without moving" (comp up insert-down))
(def append-child "Insert the item as the rightmost child of the node at this loc, without moving" (comp up append-down))

(def node z/tree)
(defn branch? [loc] (z/branches loc))
(defn children [loc] (if-let [children (z/branches loc)]
                       children
                       (throw (throw (ex-info "Called children on a leaf node" {::loc loc})))))

(defrecord GeneralZip [branch? children make-node parents]
  z/Zip
  (z-dn [_ t] (when (branch? t) [(children t) (GeneralZip. branch? children make-node (conj parents t))]))
  (z-up [_ branches] [(make-node (peek parents) branches) (GeneralZip. branch? children make-node (pop parents))]) )

;; https://insideclojure.org/2015/01/02/sequences/
(defn zipper
  "Creates a new zipper structure.  Note that zippers created using this function are not serializable.

  `branch?` is a predicate fn that returns true if the given (sub)tree can have child branches.

  `children` is a fn that, given a (sub)tree, returns a possibly empty sequence of its subtrees, or nil if it is not a branch.

  `make-node` is a constructor fn that, given a (sub)tree and a seq of branches, returns a new (sub)tree having the supplied child branches.

  `root` is the root of the tree."
  [branch? children make-node root]
  (let [z (->GeneralZip branch? children make-node [])]
    (loc/zipper z root)))

(defrecord SeqableZip []
  z/Zip
  (z-dn [this t] (when (seqable? t) [t this] ))
  (z-up [this branches] [branches this]))

(def seqable-zip ; generic; will return a tree of lists since we generally don't now how to reconstitue a seqable
  "Return a zipper for nested seqables, given a root seqable"
  (partial loc/zipper (->SeqableZip)))

(defrecord CollZip [parents]
  z/Zip
  (z-dn [this t] (when (coll? t) [(sequence t) (CollZip. (conj parents t))] ))
  (z-up [this branches] [(into (empty (peek parents)) branches) (CollZip. (pop parents))]))

(def coll-zip
  "Return a zipper for nested collections, given a root collection"
  (partial loc/zipper (->CollZip [])))

(defrecord SeqZip []
  z/Zip
  (z-dn [this t] (when (seq? t) [(sequence t) this])) ; `sequence` ensures we don't bottom out on nil
  (z-up [this branches] [branches this]))

(def seq-zip ; generic; will return a tree of lists since we generally don't now how to reconstitue a seq
  "Return a zipper for nested seqs, given a root seq"
  (partial loc/zipper (->SeqZip)))

(defrecord ListZip [parents]
  z/Zip
  (z-dn [this t] (when (list? t) [t (ListZip. (conj parents t))]))
  (z-up [this branches] [(reverse (into (empty (peek parents)) branches)) (ListZip. (pop parents))])) ; (comp reverse fill-template)

(def list-zip
  "Return a zipper for nested lists, given a root list"
  (partial loc/zipper (->ListZip [])))

(defrecord VectorZip [parents]
  z/Zip
  (z-dn [this t] (when (vector? t) [t (VectorZip. (conj parents t))]))
  (z-dn [_ t k] (when (vector? t)
                  (when-let [fulcrum (pivot/pivot t k)]
                    [fulcrum (VectorZip. (conj parents t))])))
  (z-up [this branches] [(into (empty (peek parents)) branches) (VectorZip. (pop parents))]))

(def vector-zip
  "Return a zipper for nested vectors, given a root vector"
  (partial loc/zipper (->VectorZip [])))

(defn ->me [pr] (if (map-entry? pr) pr #?(:clj (clojure.lang.MapEntry. (first pr) (second pr)) :cljs (cljs.core/MapEntry. (first pr) (second pr) nil))))

(defrecord MapZip [parents]
  z/Zip
  (z-dn [this t] (when (-> t ->me val map?) [(-> t ->me val sequence) (MapZip. (conj parents t))]))
  (z-dn [_ t k] (when (-> t ->me val map?)
                  (when-let [fulcrum (pivot/pivot (-> t ->me) k)]
                    [fulcrum (VectorZip. (conj parents t))])))
  (z-up [this branches] (let [[k v] (peek parents)]
                          [(->me [k (into (empty v) (map ->me) branches)]) (MapZip. (pop parents))])))

(def map-zip*
  "Return a zipper for nested map entries, given a root map entry"
  (partial loc/zipper (->MapZip [])))

(defn map-zip
  "Return a zipper for nested maps, given a root map of children"
  ([root-children] (map-zip ::root root-children))
  ([root-key root-children]
   (map-zip* (->me [root-key root-children]))))

(defrecord XMLZip [parents]
  z/Zip
  (z-dn [this t] (when (map? t) [(-> t :content sequence) (XMLZip. (conj parents t))]))
  (z-up [this branches] [(assoc (peek parents) :content (apply vector branches)) (XMLZip. (pop parents))]))

(def xml-zip
  "Return a zipper for xml elements (as from xml/parse), given a root element"
  (partial loc/zipper (->XMLZip [])))
