(ns advent-2019.day22
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [abs expt]]))

;;; https://adventofcode.com/2019/day/22

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; "Deal into new stack" is just a reverse operation.
(defn- deal-new-stack [deck] (reverse deck))

;; Do the cut with dram and take.
(defn- cut-n-cards [deck n]
  (if (pos? n)
    (concat (drop n deck) (take n deck))
    (cut-n-cards deck (+ (count deck) n))))

;; Kind of clumsy way to do the "deal with increment ...".
(defn- deal-increment [deck n]
  (let [size    (count deck)
        pairs   (map (fn [val idx]
                       (list idx val))
                     deck
                     (map #(mod (* n %) size) (range size)))]
    (seq (reduce (fn [d [idx val]]
                   (assoc d idx val))
                 (vec (repeat size -1)) pairs))))

;; Shuffle the deck once, based on the lines given.
(defn- shuffle-deck-once [deck lines]
  (loop [[line & lines] lines, deck deck]
    (cond
      (nil? line)                 deck
      (re-find #"deal into" line) (recur lines (deal-new-stack deck))
      (re-find #"cut " line)      (recur lines
                                         (cut-n-cards
                                          deck (Integer/parseInt
                                                (first (re-seq #"-?\d+"
                                                               line)))))
      (re-find #"deal with" line) (recur lines
                                         (deal-increment
                                          deck (Integer/parseInt
                                                (first (re-seq #"-?\d+"
                                                               line)))))
      :else
      (str "Bad line: " line))))

;; Shuffle a deck of "size", the number of times specified, based on the lines
;; given.
(defn- shuffle-deck [size times lines]
  (let [deck (range size)]
    (loop [n 0, deck deck]
      (if (= n times)
        deck
        (recur (inc n) (shuffle-deck-once deck lines))))))

;; Find the position of the specified value in the deck.
(defn- find-pos [which deck]
  (let [deck (vec deck)]
    (for [idx (range (count deck)) :when (= (deck idx) which)] idx)))

;; For part 2, have to do this totally differently...

;; Get the la and lb factors used by get-factors, below.
(defn- get-la-lb [line]
  (cond
    (re-find #"deal into" line) (list -1 -1)
    (re-find #"cut " line)      (list 1
                                      (- (Integer/parseInt
                                          (first (re-seq #"-?\d+" line)))))
    (re-find #"deal with" line) (list (Integer/parseInt
                                       (first (re-seq #"-?\d+" line)))
                                      0)))

;; Get the a and b factors that the "heavy shuffle" needs.
(defn- get-factors [size lines]
  (loop [[line & lines] lines, a 1, b 0]
    (cond
      (nil? line) (list a b)
      :else       (let [[la lb] (get-la-lb line)]
                    (recur lines
                           (mod (* la a) size)
                           (mod (+ (* la b) lb) size))))))

;; From https://rosettacode.org/wiki/Modular_exponentiation#Clojure
(defn- modpow [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

;; Fermat inv
(defn- inv [a n] (modpow a (- n 2) n))

;; Use a computational method to get the card at the "which" position, based
;; on the factors (a, b) calculated.
(defn- heavy-shuffle-find [size times which lines]
  (let [[a b] (get-factors size lines)
        Ma    (modpow a times size)
        Mb    (mod (* b (dec Ma) (inv (dec a) size)) size)]
    (mod (* (- which Mb) (inv Ma size)) size)))

;;; Problem 1
(defn p01 [file & [size]]
  (->> file
       (read-lines)
       (shuffle-deck (or size 10007) 1)
       (find-pos 2019)
       first))

;;; Problem 2
(defn p02 [file]
  (->> file
       (read-lines)
       (heavy-shuffle-find 119315717514047 101741582076661 2020)))
