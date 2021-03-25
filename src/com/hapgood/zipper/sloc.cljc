(ns com.hapgood.zipper.sloc
  (:require [com.hapgood.zipper :as zipper]))

(deftype Siblings [lefts mtree rights z]
  clojure.lang.Sequential ; just a marker...
  clojure.lang.ISeq ; ...to allow sequential destructuring
  (seq [this] (list lefts mtree rights))
  (first [this] lefts)
  zipper/Treeish
  (tree [this] (first (zipper/z-up z (map zipper/tree (concat (reverse lefts) [mtree] rights)))))
  Object
  (equals [this other] (and (= (type this) (type other)) (= [lefts mtree rights] [(.-lefts other) (.-mtree other) (.-rights other)])))
  (hashCode [_] (.hashCode [lefts mtree rights])))

(def ^:private top
  "A sentinel value representing the path of the tree at the top of a zipper"
  [() nil ()])

(defrecord Loc [t p z]
  zipper/Treeish
  (tree [this] (zipper/tree t))
  (branches [this] (first (zipper/z-dn z (zipper/tree this))))
  zipper/Zipper
  (left [this] (let [[lefts p' rights] p]
                 (when-let [[l & ls] (seq lefts)] ; fails for leftmost (thus top)
                   (->Loc l [(sequence ls) p' (cons t rights)] z))))
  (right [this] (let [[lefts p' rights] p]
                  (when-let [[r & rs] (seq rights)] ; fails for rightmost (thus top)
                    (->Loc r [(cons t lefts) p' (sequence rs)] z))))
  (up [this] (when (not= top p)
               (let [[lefts p' rights] p]
                 (->Loc (->Siblings lefts t rights z) p' z))))
  (down [this] (if (instance? Siblings t)
                 (let [[lmts mt rmts] t]
                   (->Loc mt [lmts p rmts] z))
                 (when-let [[t' z'] (zipper/z-dn z t)]
                   (when-let [[c & cs] (seq t')]
                     (->Loc c [() p (sequence cs)] z')))))
  (change [this t] (->Loc t p z))
  (insert-left [this l] (if (not= top p)
                          (let [[lefts p' rights] p
                                node [(cons l lefts) p' rights]]
                            (->Loc t node z))
                          (throw (ex-info "Can't insert left of top" {:loc this :t t}))))
  (insert-right [this r] (if (not= top p)
                           (let [[lefts p' rights] p
                                 node [lefts p' (cons r rights)]]
                             (->Loc t node z))
                           (throw (ex-info "Can't insert right of top" {:loc this :t t}))))
  (insert-down [this t1] (if (instance? Siblings t)
                           (-> this zipper/down (zipper/insert-left t1) zipper/left)
                           (if-let [[cs z'] (zipper/z-dn z t)]
                             (let [p' [() p cs]]
                               (->Loc t1 p' z'))
                             (throw (ex-info "Can only insert down from a branch" {:loc this :t t})))))
  (delete [this] (if (not= top p)
                   (let [[[l & ls :as lefts] parent [r & rs :as rights]] p]
                     (cond
                       r (->Loc r [lefts parent (sequence rs)] z)
                       l (->Loc l [(sequence ls) parent rights] z)
                       ;; Re-cast the Siblings as a mere Section
                       true (let [[t' z'] (zipper/z-up z ())]
                              (->Loc t' parent z'))))
                   (throw (ex-info "Can't remove at top" {:loc this :t t})))))

(defn zipper
  "Creates a new zipper structure.

  `z` is a value that satisfies com.hapgood.zipper/Zip and ensures that branch nodes can be opened and closed consistently.

  `root` is the root of the tree."
  [z root]
  (->Loc root top z))

(defn loc? [obj] (instance? Loc obj))

(remove-method print-dup Siblings)
(remove-method print-method Siblings)
;; (defmethod print-dup Siblings [s w] (print-ctor s (fn [s w] (print-dup (.-treeish s) w)) w))
;; (defmethod print-method Siblings [s w] (do (.write w "Siblings[")
;;                                            (print-method (.-lefts s) w)
;;                                            (.write w " ")
;;                                            (print-method (.-mtree s) w)
;;                                            (.write w " ")
;;                                            (print-method (.-rights s) w)
;;                                            (.write w "]")))

(defmethod print-dup Loc [l w] (print-ctor l (fn [s w] (print-dup (.-t l) w) (print-dup (.-p l) w)) w))
(defmethod print-method Loc [l w] (do (.write w "ScarLoc(")
                                      (print-method (.-t l) w)
                                      (.write w ", ")
                                      (print-method (.-p l) w)
                                      (.write w ")")))
