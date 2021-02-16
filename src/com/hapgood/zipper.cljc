(ns com.hapgood.zipper
  (:refer-clojure :exclude (replace remove next)))

;; Reference: https://www.st.cs.uni-saarland.de//edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf

;; Issues with implementation by Hickey
;; 1. The use of metadata "seems" less idiomatic than a protocol & defrecord.  Performance may or may not suffer (TBD) but the syntax with protocols more closely matches the intent of the code, in my opinion.
;; 2. Sentinel values like `:end` and `nil` "seem" less idiomatic than namespaced keywords and sentinel objects.
;; 3. The term `path` as the conjugate of node in the code seems misleading... it's a position, not a trail of how one got to the current position.
;; 4. There is a bug in the original implementation when deleting the last child of the root in a seq-zip.
;; 5. The functionality provided by the `pnodes` and `ppath` overlap sufficiently that just a single reference to the parent loc (ppath) suffices.
;; 6. The `root` function complects `move` and `return-updated-data-structure`.  In fact, there is no (simple) way to move to the root without also exiting the zipper.  This is surprising and it's not too hard to string together `root` followed by `node` to accomplish the combined effect.

(defprotocol Zipper
  (branch? [this] "Return true if the node at this loc is a branch")
  (children [this] "Return a seqable of the children of the node at this loc, which must be a branch")
  (make-node [this node children] "Return a new branch node, given an existing node and new children"))

(defprotocol Loc ; also requires implementation to be associative for these named accessors
  (node [this] "Return the node at this loc")
  (rights [this] "Return a seqable of the right siblings of this loc")
  (lefts [this] "Return a seqable of the left siblings of this loc")
  (parent [this] "Return the parent loc of this loc, or nil if this loc is the root")
  (dirty-generation? [this] "Return a boolean predicated on a node in this loc's generation having been changed"))

(defn- make-end
  "Create a sentinel loc that can only result from navigating beyond the limits of the data structure"
  [loc]
  (let [throw! (fn [](throw (ex-info "Operation not allowed on end loc" {::root loc})))]
    ^{::end loc} (reify Loc
                   (node [_] (throw!))
                   (rights [_] (throw!))
                   (lefts [_] (throw!))
                   (parent [_] (throw!))
                   (dirty-generation? [_] (throw!)))))

(def end?
  "Return wormholed loc if loc represents the end of a depth-first walk, otherwise nil"
  (comp ::end meta))

(defn replace
  "Replace the node at this loc, without moving"
  [loc node]
  (assoc loc :node node :dirty-generation? true))

;; Hierarchical navigation
(defn up
  "Returns the loc of the parent of the node at this loc (reflecting any changes), or nil if at the top"
  [loc] (when-let [ploc (parent loc)]
          (if (dirty-generation? loc)
            (replace ploc (make-node loc (node ploc) (concat (lefts loc) (cons (node loc) (rights loc)))))
            ploc)))

(defn down
  "Return the loc of the leftmost child of the node at this loc, or nil if no children"
  [loc]
  (when (branch? loc)
    (let [[child & children] (children loc)]
      (when child
        (assoc loc :node child :lefts [] :parent loc :rights (or children ()) :dirty-generation? false)))))

(defn root
  "Zip all the way up and return the root node, reflecting any changes."
  [loc]
  (or (end? loc) (if-let [p (up loc)] (root p) loc)))

(defn path
  "Return a seq of nodes leading to this loc"
  [loc]
  (if-let [ploc (parent loc)]
    (conj (path ploc) (node ploc))
    []))

;; Ordered navigation
(defn left
  "Return the loc of the left sibling of the node at this loc, or nil"
  [loc]
  (let [lefts (lefts loc)]
    (when (seq lefts)
      (assoc loc :node (peek lefts) :lefts (pop lefts) :rights (cons (node loc) (rights loc))))))

(defn right
  "Return the loc of the right sibling of the node at this loc, or nil"
  [loc]
  (when-let [[r & rs :as rights] (seq (rights loc))]
    (assoc loc :node r :lefts (conj (lefts loc) (node loc)) :rights (or rs ()))))

(defn leftmost
  "Returns the loc of the leftmost sibling of the node at this loc, or self"
  [loc]
  (if-let [lefts (seq (lefts loc))]
    (assoc loc :node (first lefts) :lefts [] :rights (concat (rest lefts) [(node loc)] (rights loc)))
    loc))

(defn rightmost
  "Returns the loc of the leftmost sibling of the node at this loc, or self"
  [loc]
  (if-let [rights (seq (rights loc))]
    (assoc loc :node (last rights) :lefts (apply conj (lefts loc) (node loc) (butlast rights)) :rights [])
    loc))

(defn insert-left
  "Insert the item as the left sibling of the node at this loc, without moving"
  [loc item]
  (if (empty? (path loc))
    (throw (new Exception "Insert at top"))
    (assoc loc :lefts (conj (lefts loc) item) :dirty-generation? true)))

(defn insert-right
  "Insert the item as the right sibling of the node at this loc, without moving"
  [loc item]
  (if (empty? (path loc))
    (throw (new Exception "Insert at top"))
    (assoc loc :rights (cons item (rights loc)) :dirty-generation? true)))

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
       (if-let [p' (up p)]
         (or (right p') (recur p'))
         (make-end p))))))

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
  "Remove the node at loc, returning the loc that would have preceded it in a depth-first walk."
  [loc]
  (let [lefts (lefts loc)]
    (if (empty? (path loc))
      (throw (new Exception "Remove at top"))
      (if (pos? (count lefts))
        (loop [loc (assoc loc :node (peek lefts) :lefts (pop lefts) :dirty-generation? true)]
          (if-let [child (and (branch? loc) (down loc))]
            (recur (rightmost child))
            loc))
        (replace (parent loc) (make-node loc (node (parent loc)) (rights loc)))))))

(defrecord ListZipper [node lefts parent rights dirty-generation?]
  Zipper
  (branch? [this] (list? node))
  (children [this] node)
  (make-node [this node children] (reverse (into (empty node) children)))
  Loc
  (node [this] node)
  (lefts [this] lefts)
  (rights [this] rights)
  (parent [this] parent)
  (dirty-generation? [this] dirty-generation?))

(defn list-zip
  "Return a zipper for nested lists, given a root list"
  [root] (->ListZipper root () nil () false))

(defrecord MapZipper [node lefts parent rights dirty-generation?]
  Zipper
  (branch? [this] ((comp map? val) node))
  (children [this] (val node))
  (make-node [this [k children :as node] children'] (clojure.lang.MapEntry. k (into (empty children) children')))
  Loc
  (node [this] node)
  (lefts [this] lefts)
  (rights [this] rights)
  (parent [this] parent)
  (dirty-generation? [this] dirty-generation?))

(defn map-zip
  "Return a zipper for nested maps, given a root map"
  ([root] (map-zip ::map-zip-root root))
  ([root-key root] (->MapZipper (clojure.lang.MapEntry. root-key root) [] nil [] false)))

(defrecord VectorZipper [node lefts parent rights dirty-generation?]
  Zipper
  (branch? [this] (vector? node))
  (children [this] node)
  (make-node [this children children'] (into (empty children) children'))
  Loc
  (node [this] node)
  (lefts [this] lefts)
  (rights [this] rights)
  (parent [this] parent)
  (dirty-generation? [this] dirty-generation?))

(defn vector-zip
  "Return a zipper for nested vectors, given a root vector"
  [root] (->VectorZipper root []  nil [] false))

(defprotocol ChildrenByName
  (pivot [loc cname] "Return a triple of [children-before pivot-child children-after"))

(extend-protocol ChildrenByName
  MapZipper
  (pivot [this k]
    (let [children (children this)]
      (when-let [pivot (find children k)]
        (let [v (vec children)
              i (.indexOf (keys children) k)]
          [(into {} (subvec v 0 i)) pivot (into {} (subvec v (inc i)))]))))
  VectorZipper
  (pivot [this k]
    (let [children (children this)]
      (when-let [[k v] (find children k)]
        [(subvec children 0 k) v (subvec children (inc k))]))))

(defn down-to
  "Return the loc of the child named by `k`"
  [loc k]
  (when (branch? loc)
    (when-let [[lefts node rights] (pivot loc k)]
      (assoc loc :node node :lefts lefts :parent loc :rights rights :dirty-generation? false))))
