(ns com.hapgood.zipper)

;; Original Huet implementation
(defprotocol Zipper
  (left [loc] "Return the loc of the left sibling of the tree at this loc, or nil.")
  (right [loc] "Return the loc of the right sibling of the tree at this loc, or nil.")
  (up [loc] "Return the loc of the parent of the tree at this loc (reflecting any changes), or nil if at the top.")
  (down [loc] "Return the loc of the leftmost child of the tree at this loc, or nil if no children.")
  (change [loc t] "Replace the tree at this loc with t, without moving.")
  (insert-left [loc t] "Insert t as the left sibling of the tree at this loc, without moving.")
  (insert-right [loc t] "Insert t as the right sibling of the tree at this loc, without moving.")
  (insert-down [loc t] "Insert t as the leftmost child of the tree (which must be a branch) at this loc, moving to the newly inserted t.")
  (delete [loc] "Delete the tree at loc, returning the loc that would have preceded it in a depth-first walk."))

(defprotocol TreeLike
  (tree [this] "Return the pure treeish data structure")
  (branch? [this] "Can this tree-like have branches?")
  ;; (= treelike (seed treelike (branches treelike)))
  (branches [this] "Return a seq of the child tree-like branches of this tree-like, or nil if it is not a branch")
  (seed [this branches] "Return a new tree-like with the same treeish genesis but with the supplied tree-like branches"))

(extend-protocol TreeLike
  nil
  (tree [this] this)
  (branch? [_] false)
  (branches [this] nil)
  (seed [this _] (throw (ex-info "Leaf objects cannot seed a new tree" {:obj this})))
  Object
  (tree [this] this)
  (branch? [_] false)
  (branches [this] nil)
  (seed [this _] (throw (ex-info "Leaf objects cannot seed a new tree" {:obj this}))))

(defn nth-child [loc n] (cond
                          (zero? n) (throw (ex-info "Only positive integers allowed to identify the nth child." {:loc loc :n n}))
                          (= 1 n) (down loc)
                          true (right (nth-child loc (dec n)))))
