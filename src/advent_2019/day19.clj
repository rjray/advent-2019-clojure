(ns advent-2019.day19
  (:require [advent-2019.intcode :as ic]))

;;; https://adventofcode.com/2019/day/19

;; Read the given file as a stream of comma-separated data values. Return a
;; sequence of the numerical values.
(defn- read-opcodes [file]
  (->> file
       (slurp)
       (re-seq #"-?\d+")
       (map #(Long/parseLong %))))

;; Test whether grid-point (x y) is affected by the tractor beam. This never
;; changes for a given (x y), so we can use memoize to speed this up.
(def is-affected
  (memoize (fn [m x y]
             (ic/get-output (ic/execute (reduce (fn [m i]
                                                  (ic/add-input m i))
                                                m (list x y)))))))

;; A boolean predicate based on the above, for cases where we need true/false
;; rather than 1/0.
(defn- is-affected? [m x y]
  (not (zero? (is-affected m x y))))

;; For part 1, count all the affected grid points in the first 50x50 block.
(defn- count-affected [m]
  (apply + (for [y (range 50), x (range 50)]
             (is-affected m x y))))

;; Used to validate the (potential) answer for part 2. Makes sure all 4 points
;; of the 100x100 box are affected.
(defn- check-answer [x y m]
  (and (is-affected? m x y)
       (is-affected? m (+ x 99) y)
       (is-affected? m x (+ y 99))
       (is-affected? m (+ x 99) (+ y 99))))

;; Calculate the x-index of where the beam starts for all y values up to 10000.
(defn- calc-left-edges [m from to]
  (loop [dists {},  x from, y 0]
    (cond
      (= x to) dists
      (is-affected? m x y) (recur (assoc dists x y) (inc x) y)
      :else                (recur dists x (inc y)))))

;; Find the top-left coordinate, using the table of x-index/distances created in
;; the previous fn.
(defn- find-top-left [m from to dists]
  (loop [x 0, y from]
    (cond
      (= y to) (throw (AssertionError. "Could not find y before 10000"))
      (not (is-affected? m x y))               (recur (inc x) y)
      (<= (get dists (+ x 99) 10000) (- y 99)) (list x (get dists (+ x 99)))
      :else                                    (recur x (inc y)))))

;; Find the upper-left corner of the first 100x100 square that fits fully
;; within the beam.
(defn- find-square [m]
  (let [dists (calc-left-edges m 10 10000)
        [x y] (find-top-left m 10 10000 dists)]
    (if (check-answer x y m) (list x y) (list 0 0))))

;;; Problem 1
(defn p01 [file]
  (->> file
       (read-opcodes)
       (ic/initialize-machine)
       (count-affected)))

;;; Problem 2
(defn p02 [file]
  (->> file
       (read-opcodes)
       (ic/initialize-machine)
       (find-square)
       ((fn [[x y]] (+ y (* 10000 x))))))
