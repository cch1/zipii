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

(defprotocol Treeish
  (tree [this] "Return the pure treeish data structure")
  (branches [this] "Return a seq of the child branches of this tree, or nil if it is a leaf (not a branch)"))

(extend-protocol Treeish
  nil
  (tree [this] this)
  Object
  (tree [this] this))

(defprotocol Zip
  "Perform up-and-down zip operations.  Like a stack, up is guaranteed to never be called without a preceeding dn"
  (z-dn [this t] [this t k] "Return a pair of a sequence of child branches (or a three-tuple pivoting on k) and a possibly updated Zip, or nil if t is not a branch.")
  (z-up [this branches] "Return a pair of a new tree with with the supplied branches and a possibly updated Zip"))

(defn nth-child [loc n] (cond
                          (zero? n) (throw (ex-info "Only positive integers allowed to identify the nth child." {:loc loc :n n}))
                          (= 1 n) (down loc)
                          true (right (nth-child loc (dec n)))))
