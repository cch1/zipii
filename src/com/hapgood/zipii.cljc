(ns com.hapgood.zipii
  (:refer-clojure :exclude (replace remove next))
  (:require [com.hapgood.zipper.loc :as loc]
            [com.hapgood.zipper.section :refer [make->treeish]]
            [com.hapgood.zipper :as z]))

;; Reference: https://www.st.cs.uni-saarland.de//edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf

;; Issues with implementation by Hickey:
;; 1. The use of metadata "seems" less idiomatic than a protocol & defrecord.  Performance may or may not suffer (TBD) but the syntax with protocols more closely matches the intent of the code, in my opinion.
;; 2. Sentinel values like `:end` and `nil` "seem" less idiomatic than namespaced keywords and sentinel objects.
;; 4. There is a bug in the original implementation when deleting the last child of a node in a seq-zip.
;; 5. The `root` function complects `move` and `return-updated-data-structure`.  In fact, there is no (simple) way to move to the root without also exiting the zipper.  This is surprising and it's not too hard to string together `root` followed by `item` to accomplish the combined effect.
;; 6. The `changed?` attribute increases performance relative to the base Huet implementation (in which the tree is reconstituted when zipping up regardless of changes) when movement is more vertical, but at the expense of complexity.  I will explore scars for this purpose at some later time.
;; 7. The vocabulary diverges from Huet and while Hickey's seems more reasonable, the Huet paper is an excellent implementation guide and sticking to his vocabulary reinforces that effect.
;; 8. Exceptions are not very precise nor are they data-laden.

(def left z/left)
(def right z/right)
(def up z/up)
(def down z/down)
(def change z/change)
(def insert-left z/insert-left)
(def insert-right z/insert-right)
(def insert-down z/insert-down)
(def delete z/delete)
(def branch? z/branch?)

(def nth-child z/nth-child)

;; RH/clojure.zip compatibility: shims and extensions

;; Iterative navigation
(defn- make-end
  "Create a sentinel loc that can only result from navigating beyond the limits of the data structure"
  [loc]
  (let [throw! (fn [](throw (ex-info "Operation not allowed on end loc" {::root loc})))]
    ^{::end loc} (reify
                   z/TreeLike
                   (tree [loc] (throw!))
                   (branch? [loc] (throw!))
                   (branches [loc] (throw!))
                   (seed [loc branches] (throw!))
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
  "Zip all the way up and return the loc of the root, reflecting any changes."
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

(defn- iteratively [n f]
  "Return a function that composes n applications of f"
  (fn [x] (loop [i n x x]
            (if (zero? i) x (recur (dec i) (f x))))))

(defn- slew
  "Slew the first loc to the DFS position of the second, presuming loc1's DFS predecessors have not been changed in loc0's tree"
  ;; When loc0 and loc1 have the same parent and common elder siblings they are, by this definition, in the same position.
  ;; NB: this condition will not hold if either loc has zip-propogated independent changes up to a shared ancestor.
  ;; The intent of this operation is to "pull" loc0 to a DFS predecessor in its tree, dragging along any zipped changes loc0 may embody.
  ;; To prevent independent changes from being zipped into a parent (and changing their identity), the strategy is to move both locs up
  ;; until they have identical parents and then move loc0 back through the recorded path of show loc1 arrived at the common ancestor.
  [loc0 loc1]
  (loop [loc0 loc0 loc1 loc1 s identity]
    (let [[lefts0 parent0 _]  (:p loc0)
          [lefts1 parent1 _]  (:p loc1)
          ls1 (count lefts1)]
      (if (= parent0 parent1)
        (if (= lefts0 lefts1)
          (s loc0)  ; replay on loc0 how loc1 got here
          (recur (leftmost loc0) (leftmost loc1) (comp s (iteratively ls1 right))))
        (let [ds0 (count (:pts loc0))
              ds1 (count (:pts loc1))
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
(defn children [loc] (map z/tree (z/branches loc)))
(def make-node z/seed)

;; https://insideclojure.org/2015/01/02/sequences/
(defn zipper
  "Creates a new zipper structure.

  `branch?` is a predicate fn that returns true if the given (sub)tree can have child branches.

  `children` is a fn that, given a (sub)tree, returns a possibly empty sequence of its subtrees, or nil if it is not a branch.

  `make-node` is a constructor fn that, given a (sub)tree and a seq of branches, returns a new (sub)tree having the supplied child branches.

  `root` is the root of the tree."
  [branch? children make-node root]
  (let [->treeish (make->treeish (fn [t] (when (branch? t) (children t))) make-node)]
    (loc/zipper ->treeish root)))

(defn- fill-template [^clojure.lang.IPersistentCollection tree children] (into (empty tree) children))

(def seqable-zip ; generic; mutation will return a tree of lists since we don't now how to fill a template
  "Return a zipper for nested seqables, given a root seqable"
  (partial zipper seqable? identity (fn [tree children] children)))

(def coll-zip
  "Return a zipper for nested collections, given a root collection"
  (partial zipper coll? identity fill-template))

(def seq-zip
  "Return a zipper for nested seqs, given a root seq"
  (partial zipper seq? identity fill-template))

(def list-zip
  "Return a zipper for nested lists, given a root list"
  ;; The section fn needs to reverse after filling an empty list
  (partial zipper list? identity (comp reverse fill-template)))

(def vector-zip
  "Return a zipper for nested vectors, given a root vector"
  (partial zipper vector? identity fill-template))

(defn ->me [pr] (if (instance? clojure.lang.MapEntry pr) pr (clojure.lang.MapEntry. (first pr) (second pr) )))

(def map-zip*
  "Return a zipper for nested map entries, given a root map entry"
  (let [section (fn [[k children :as tree] children'] (clojure.lang.MapEntry. k (into (empty children) (map ->me) children')))]
    (partial zipper (comp map? val ->me) (comp sequence val ->me) section)))

(defn map-zip
  "Return a zipper for nested maps, given a root map of children"
  ([root-children] (map-zip ::root root-children))
  ([root-key root-children]
   (map-zip* (clojure.lang.MapEntry. root-key root-children))))

(def xml-zip
  "Return a zipper for xml elements (as from xml/parse), given a root element"
  (let [section (fn [tree children'] (assoc tree :content (apply vector children')))]
    (partial zipper (partial instance? clojure.lang.IPersistentMap) (comp sequence :content) section)))
