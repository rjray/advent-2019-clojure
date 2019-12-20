;;; heap.clj
;;;
;;; An implementation of both min-heaps and max-heaps. Each heap is presented
;;; as a vector with the content stored from index 1. Index 0 is used to store
;;; a map that holds the functions to be used in comparing elements for the
;;; "bubble-up" and "heapify" operations.
(ns advent-2019.heap)

;; Swap two elements of the given vector, using assoc! on the assumption that
;; the vector is currently transient.
(defn- swap [v i j] (assoc! v i (v j) j (v i)))

;; Return the parent index of the given index i
(defn- parent [i] (quot i 2))

;; Return the index of the left child of the element at index i
(defn- left-child [i] (* i 2))

;; Return the index of the right child of the element at index i
(defn- right-child [i] (inc (* i 2)))

;; Return the index of the largest-valued element from the perspective of
;; the parent at i. Value will be i or one of the children, whichever has
;; the largest key-value.
(defn- largest [heap i]
  (let [left      (left-child i)
        left-key  (get-in heap [left :key])
        right     (right-child i)
        right-key (get-in heap [right :key])
        root-key  (get-in heap [i :key])
        size      (count heap)
        largest   (if (and (< left size)
                           (= 1 (compare left-key root-key))) left i)]
    (if (and (< right size)
             (= 1 (compare right-key (get-in heap [largest :key]))))
      right largest)))

;; Return the index of the smallest-valued element from the perspective of
;; the parent at i. Value will be i or one of the children, whichever has
;; the smallest key-value.
(defn- smallest [heap i]
  (let [left      (left-child i)
        left-key  (get-in heap [left :key])
        right     (right-child i)
        right-key (get-in heap [right :key])
        root-key  (get-in heap [i :key])
        size      (count heap)
        smallest  (if (and (< left size)
                           (= -1 (compare left-key root-key))) left i)]
    (if (and (< right size)
             (= -1 (compare right-key (get-in heap [smallest :key]))))
      right smallest)))

;; Compare the key-values of the two elements to see whether a is less than
;; b.
(defn- keys< [a b] (= -1 (compare (:key a) (:key b))))

;; Compare the key-values of the two elements to see whether a is greater than
;; b.
(defn- keys> [a b] (= 1 (compare (:key a) (:key b))))

;; This map is assigned to slot 0 of any min-heap.
(def ^:private min-heap-map {:compare keys>, :choose smallest})

;; This map is assigned to slot 0 of any max-heap.
(def ^:private max-heap-map {:compare keys<, :choose largest})

;; Re-arrange the tree from node i downward as needed to set the heap property.
;; Uses the "choose" key from the heap's map to decide whether to bubble-down
;; the largest or smallest key.
(defn- heapify [heap i]
  (let [downto (get-in heap [0 :choose])]
    (loop [heap (transient heap), i i]
      (let [l (downto heap i)]
        (cond
          (= i l) (persistent! heap)
          :else   (recur (swap heap i l) l))))))

;; Build a heap (type determined by the empty heap passed in) from a given
;; collection of elements.
(defn- build-heap [heap s]
  (let [size (count s)
        heap (into heap s)]
    (loop [heap heap, i (quot size 2)]
      (cond
        (zero? i) heap
        :else     (recur (heapify heap i) (dec i))))))

;; Public-facing function. Return the element currently at the root of the
;; heap.
(defn root [heap] (get heap 1))

;; Public-facing function. Insert a new element into the given heap. Uses
;; "compare" key from the heap's map to decide whether to bubble-up the
;; larger or smaller key.
(defn insert [heap elt]
  (let [heap  (conj heap elt)
        move? (get-in heap [0 :compare])]
    (loop [heap (transient heap), k (dec (count heap))]
      (let [p (parent k)]
        (cond
          (zero? p)                 (persistent! heap)
          (move? (heap p) (heap k)) (recur (swap heap p k) p)
          :else                     (persistent! heap))))))

;; Public-facing function. Delete the root element from the given heap. Calls
;; (heapify) to restore the heap property of the structure.
(defn delete [heap]
  (cond
    (= 1 (count heap)) heap
    :else
    (let [last-elt (dec (count heap))
          heap     (pop (assoc heap 1 (heap last-elt)))]
      (heapify heap 1))))

;; Public-facing function. Create and return a min-heap, possibly initializing
;; it with a given collection.
(defn min-heap
  ([]  (vec (list min-heap-map)))
  ([s] (build-heap (min-heap) s)))

;; Public-facing function. Create and return a max-heap, possibly initializing
;; it with a given collection.
(defn max-heap
  ([]  (vec (list max-heap-map)))
  ([s] (build-heap (max-heap) s)))

;; Public-facing function. Return the current size of the heap.
(defn size [heap] (dec (count heap)))
