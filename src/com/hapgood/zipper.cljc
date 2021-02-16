(ns com.hapgood.zipper
  (:refer-clojure :exclude (replace remove next)))

;; Reference: https://www.st.cs.uni-saarland.de//edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf

;; Issues with implementation by Hickey
;; 1. The use of metadata "seems" less idiomatic than a protocol & defrecord.  Performance may or may not suffer (TBD) but the syntax with protocols more closely matches the intent of the code, in my opinion.
;; 2. Sentinel values like `:end` and `nil` "seem" less idiomatic than namespaced keywords and sentinel objects.
;; 4. There is a bug in the original implementation when deleting the last child of the root in a seq-zip.
;; 5. The functionality provided by the `pnodes` and `ppath` overlap sufficiently that just a single reference to the parent loc (ppath) suffices.
;; 6. The `root` function complects `move` and `return-updated-data-structure`.  In fact, there is no (simple) way to move to the root without also exiting the zipper.  This is surprising and it's not too hard to string together `root` followed by `item` to accomplish the combined effect.

(defprotocol Zipper
  (branch? [this] "Return true if the tree of this loc is a branch")
  (children [this] "Return a seqable of the children of the tree of this loc, which must be a branch")
  (make-tree [this tree children] "Return a new subtree, given an existing subtree and new children")
  (tree [this] "Return the tree of this loc")
  (dirty-generation? [this] "Return a boolean predicated on a tree in this loc's generation having been changed"))

(defprotocol Node ; also requires implementation to be associative for these named accessors
  (lefts [this] "Return a seqable of the left siblings of tree of this loc")
  (parent [this] "Return the parent loc of this loc, or nil if this loc is the root")
  (rights [this] "Return a seqable of the right siblings of the tree of this loc"))

(defn- make-end
  "Create a sentinel loc that can only result from navigating beyond the limits of the data structure"
  [loc]
  (let [throw! (fn [](throw (ex-info "Operation not allowed on end loc" {::root loc})))]
    ^{::end loc} (reify Node
                   (lefts [_] (throw!))
                   (parent [_] (throw!))
                   (rights [_] (throw!)))))

(def end?
  "Return wormholed loc if loc represents the end of a depth-first walk, otherwise nil"
  (comp ::end meta))

(defn replace
  "Replace the tree at this loc, without moving"
  [loc tree]
  (assoc loc :tree tree :dirty-generation? true))

;; Hierarchical navigation
(defn up
  "Returns the loc of the parent of the tree at this loc (reflecting any changes), or nil if at the top"
  [loc] (when-let [ploc (parent loc)]
          (if (dirty-generation? loc)
            (replace ploc (make-tree loc (tree ploc) (concat (lefts loc) (cons (tree loc) (rights loc)))))
            ploc)))

(defn down
  "Return the loc of the leftmost child of the tree at this loc, or nil if no children"
  [loc]
  (when (branch? loc)
    (let [[child & children] (children loc)]
      (when child
        (assoc loc :tree child :lefts [] :parent loc :rights (or children ()) :dirty-generation? false)))))

(defn root
  "Zip all the way up and return the loc of the root, reflecting any changes."
  [loc]
  (or (end? loc) (if-let [p (up loc)] (root p) loc)))

(defn path
  "Return a seq of trees leading to this loc"
  [loc]
  (if-let [ploc (parent loc)]
    (conj (path ploc) (tree ploc))
    []))

;; Ordered navigation
(defn left
  "Return the loc of the left sibling of the tree at this loc, or nil"
  [loc]
  (let [lefts (lefts loc)]
    (when (seq lefts)
      (assoc loc :tree (peek lefts) :lefts (pop lefts) :rights (cons (tree loc) (rights loc))))))

(defn right
  "Return the loc of the right sibling of the tree at this loc, or nil"
  [loc]
  (when-let [[r & rs :as rights] (seq (rights loc))]
    (assoc loc :tree r :lefts (conj (lefts loc) (tree loc)) :rights (or rs ()))))

(defn leftmost
  "Returns the loc of the leftmost sibling of the tree at this loc, or self"
  [loc]
  (if-let [lefts (seq (lefts loc))]
    (assoc loc :tree (first lefts) :lefts [] :rights (concat (rest lefts) [(tree loc)] (rights loc)))
    loc))

(defn rightmost
  "Returns the loc of the leftmost sibling of the tree at this loc, or self"
  [loc]
  (if-let [rights (seq (rights loc))]
    (assoc loc :tree (last rights) :lefts (apply conj (lefts loc) (tree loc) (butlast rights)) :rights [])
    loc))

(defn insert-left
  "Insert the item as the left sibling of the tree at this loc, without moving"
  [loc item]
  (if (empty? (path loc))
    (throw (new Exception "Insert at top"))
    (assoc loc :lefts (conj (lefts loc) item) :dirty-generation? true)))

(defn insert-right
  "Insert the item as the right sibling of the tree at this loc, without moving"
  [loc item]
  (if (empty? (path loc))
    (throw (new Exception "Insert at top"))
    (assoc loc :rights (cons item (rights loc)) :dirty-generation? true)))

(defn edit
  "Replace the tree at this loc with the value of (f tree args)"
  [loc f & args]
  (replace loc (apply f (tree loc) args)))

(defn insert-child
  "Insert the item as the leftmost child of the tree at this loc, without moving"
  [loc item]
  (replace loc (make-tree loc (tree loc) (cons item (children loc)))))

(defn append-child
  "Inserts the item as the rightmost child of the tree at this loc, without moving"
  [loc item]
  (replace loc (make-tree loc (tree loc) (concat (children loc) [item]))))

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
  "Remove the tree at loc, returning the loc that would have preceded it in a depth-first walk."
  [loc]
  (let [lefts (lefts loc)]
    (if (empty? (path loc))
      (throw (new Exception "Remove at top"))
      (if (pos? (count lefts))
        (loop [loc (assoc loc :tree (peek lefts) :lefts (pop lefts) :dirty-generation? true)]
          (if-let [child (and (branch? loc) (down loc))]
            (recur (rightmost child))
            loc))
        (replace (parent loc) (make-tree loc (tree (parent loc)) (rights loc)))))))

(defrecord ListZipper [tree lefts parent rights dirty-generation?]
  Zipper
  (branch? [this] (list? tree))
  (children [this] tree)
  (make-tree [this tree children] (reverse (into (empty tree) children)))
  (tree [this] tree)
  (dirty-generation? [this] dirty-generation?)
  Node
  (lefts [this] lefts)
  (rights [this] rights)
  (parent [this] parent))

(defn list-zip
  "Return a zipper for nested lists, given a root list"
  [root] (->ListZipper root () nil () false))

(defrecord SeqableZipper [tree lefts parent rights dirty-generation?]
  Zipper
  (branch? [this] (seqable? tree))
  (children [this] (seq tree))
  (make-tree [this tree children] (into (empty tree) children))
  (tree [this] tree)
  (dirty-generation? [this] dirty-generation?)
  Node
  (lefts [this] lefts)
  (rights [this] rights)
  (parent [this] parent))

(defn seqable-zip
  "Return a zipper for nested seqables, given a root seqable"
  [root] (->SeqableZipper root [] nil [] false))

(defrecord MapZipper [tree lefts parent rights dirty-generation?]
  Zipper
  (branch? [this] ((comp map? val) tree))
  (children [this] (val tree))
  (make-tree [this [k children :as tree] children'] (clojure.lang.MapEntry. k (into (empty children) children')))
  (tree [this] tree)
  (dirty-generation? [this] dirty-generation?)
  Node
  (lefts [this] lefts)
  (rights [this] rights)
  (parent [this] parent))

(defn map-zip
  "Return a zipper for nested maps, given a root map"
  ([root] (map-zip ::map-zip-root root))
  ([root-key root] (->MapZipper (clojure.lang.MapEntry. root-key root) [] nil [] false)))

(defrecord VectorZipper [tree lefts parent rights dirty-generation?]
  Zipper
  (branch? [this] (vector? tree))
  (children [this] tree)
  (make-tree [this children children'] (into (empty children) children'))
  (tree [this] tree)
  (dirty-generation? [this] dirty-generation?)
  Node
  (lefts [this] lefts)
  (rights [this] rights)
  (parent [this] parent))

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
    (when-let [[lefts pivot rights] (pivot loc k)]
      (assoc loc :tree pivot :lefts lefts :parent loc :rights rights :dirty-generation? false))))
