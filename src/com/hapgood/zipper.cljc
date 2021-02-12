(ns com.hapgood.zipper
  (:refer-clojure :exclude (replace remove next))
  (:require [clojure.zip :as z]))

;; Issues with original implementation
;; 1. The `changed?` attribute is not carefully propogated when moving laterally, which is unexpected & inefficient when zipping up "changes".
;; 2. The use of metadata seems less idiomatic than a protocol & defrecord.  Performance may or may not suffer -TBD.
;; 3. Sentinel values like `:end` and `nil` are less idiomatic than namespaced keywords.
;; 4. The term `path` as the conjugate of node in the code is misleading... it's a position, not just a trail of how one got to the current position.

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defprotocol Zipper
  (branch? [this] "Returns true if the node at this loc is a branch")
  (children [this] "Returns a seq of the children of the node at this loc")
  (make-node [this node children] "Returns a new branch node, given an existing node and new children")
  (node [this] "Return the node at this loc")
  (rights [this] "Return a seq of the right siblings of this loc")
  (lefts [this] "Return a seq of the left siblings of this loc")
  (parent [this] "Return the parent loc of this loc, or nil if this loc is the root")
  (changed? [this] "Return a boolean predicated on this loc having been modified since being added to its parent"))

(defrecord SeqZipper [node lefts parent rights changed?]
  Zipper
  (branch? [this] (seq? node))
  (children [this] node)
  (make-node [this node children] (with-meta children (meta node)))
  (node [this] node)
  (rights [this] rights)
  (lefts [this] lefts)
  (parent [this] parent)
  (changed? [this] changed?))

(defn seq-zip [root] (->SeqZipper root [] nil [] false))

;; Sentinels
(defn end? "Returns true if loc represents the end of a depth-first walk" [loc] (= ::end (parent loc)))

;; Hierarchical navigation
(defn up
  "Returns the loc of the parent of the node at this loc, or nil if at the top"
  [loc] (when-let [ploc (parent loc)]
          (if (changed? loc)
            (assoc ploc :node (make-node loc (node ploc) (concat (lefts loc) (cons (node loc) (rights loc)))) :changed? true)
            ploc)))

(defn down [loc] (when (branch? loc)
                   (let [[child & children] (children loc)]
                     (when child
                       (->SeqZipper child [] loc (or children ()) false)))))

(defn root [loc] (if-let [p (up loc)] (root p) loc))

(defn path [loc]
  (if-let [ploc (parent loc)]
    (conj (path ploc) (node ploc))
    []))

;; Ordered navigation
(defn left [loc] (let [lefts (lefts loc)]
                   (when (seq lefts)
                     (->SeqZipper (peek lefts) (pop lefts) (parent loc) (cons (node loc) (rights loc)) false))))

(defn right [loc] (when-let [[r & rs :as rights] (seq (rights loc))]
                    (->SeqZipper r (conj (lefts loc) (node loc)) (parent loc) (or rs ()) false)))

(defn leftmost
  "Returns the loc of the leftmost sibling of the node at this loc, or self"
  [loc]
  (if-let [lefts (seq (lefts loc))]
    (->SeqZipper (first lefts) [] (parent loc) (concat (rest lefts) [(node loc)] (rights loc)) false)
    loc))

(defn rightmost
  "Returns the loc of the leftmost sibling of the node at this loc, or self"
  [loc]
  (if-let [rights (seq (rights loc))]
    (->SeqZipper (last rights) (apply conj (lefts loc) (node loc) (butlast rights)) (parent loc) [] false)
    loc))

(defn insert-left
  "Insert the item as the left sibling of the node at this loc, without moving"
  [loc item]
  (if (empty? (path loc))
    (throw (new Exception "Insert at top"))
    (assoc loc :lefts (conj (lefts loc) item) :changed? true)))

(defn insert-right
  "Insert the item as the right sibling of the node at this loc, without moving"
  [loc item]
  (if (empty? (path loc))
    (throw (new Exception "Insert at top"))
    (assoc loc :rights (cons item (rights loc)) :changed? true)))

(defn replace
  "Replace the node at this loc, without moving"
  [loc node]
  (assoc loc :node node :changed? true))

(defn edit
  "Replace the node at this loc with the value of (f node args)"
  [loc f & args]
  (replace loc (apply f (node loc) args)))

(defn insert-child
  "Insert the item as the leftmost child of the node at this loc, without moving"
  [loc item]
  (replace loc (make-node loc (node loc) (cons item (children loc)))))

(defn append-child
  "Inserts the item as the rightmost child of the node at this loc, without moving"
  [loc item]
  (replace loc (make-node loc (node loc) (concat (children loc) [item]))))

(defn next
  "Moves to the next loc in the hierarchy, depth-first. When reaching the end, returns
  a distinguished loc detectable via end?.  If already at the end, stays there."
  [loc]
  (if (end? loc)
    loc
    (or
     (and (branch? loc) (down loc))
     (right loc)
     (loop [p loc]
       (if (up p)
         (or (right (up p)) (recur (up p)))
         (assoc p :parent ::end))))))

(defn prev
  "Moves to the previous loc in the hierarchy, depth-first. If already at the root, returns nil."
  [loc]
  (if-let [lloc (left loc)]
    (loop [loc lloc]
      (if-let [child (and (branch? loc) (down loc))]
        (recur (rightmost child))
        loc))
    (up loc)))

(defn remove
  "Removes the node at loc, returning the loc that would have preceded it in a depth-first walk."
  [loc]
  (let [lefts (lefts loc)]
    (if (empty? (path loc))
      (throw (new Exception "Remove at top"))
      (if (pos? (count lefts))
        (loop [loc (assoc loc :node (peek lefts) :lefts (pop lefts) :changed? true)]
          (if-let [child (and (branch? loc) (down loc))]
            (recur (rightmost child))
            loc))
        (assoc (parent loc) :node (make-node loc (node (parent loc)) (rights loc)) :changed? true)))))
