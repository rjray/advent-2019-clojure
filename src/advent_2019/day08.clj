(ns advent-2019.day08
  (:require [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/8

;; Convenience map for converting ASCII numbers to their integers
(def ^:private num-map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})

;; Take a layer and return a 3-tuple of the count of 0's, 1's and 2's.
(defn- count-012 [layer]
  (let [counts (group-by identity layer)]
    (list (count (counts 0)) (count (counts 1)) (count (counts 2)))))

;; Solver for problem 1: Find the layer with the fewest zeros and return the
;; product of the number of ones and twos.
(defn- solve-for-min-zeros [x y data]
  (let [layer-size (* x y)
        layers     (partition layer-size data)]
    (->> layers
         (map count-012)
         (sort #(compare (first %1) (first %2)))
         (first)
         (rest)
         (apply *))))

;; For the sequence of layers, return a sequence of all the pos'th elements in
;; a new sequence of its own. This amounts to the "stack" of values at the given
;; pixel-position over all the layers.
(defn- get-vals [layers pos] (map #(% pos) layers))

;; For a stack of values, get the first non-transparent value. This is the first
;; non-2 value.
(defn- get-value [stack] (first (filter #(not= 2 %) stack)))

;; Turn the zeros to " " and the ones to "*", so that it's easier to read when
;; printed out.
(defn- zero-to-spc [line]
  (map #(if (zero? %) " " "*") line))

;; Display the "raster lines" of the image.
(defn- display [lines]
  (let [lines (map zero-to-spc lines)]
    (println (str/join "\n" (map #(str/join %) lines)))))

;; Decode the image data into layers, which become stacks of (0|1|2), which then
;; become pixels that are one of [0,1]. The sequence of pixels are then
;; partitioned into lines of x pixels and passed to (display).
(defn- decode-image [x y data]
  (let [layer-size (* x y)
        layers     (map vec (partition layer-size data))
        stacks     (map #(get-vals layers %) (range layer-size))
        pixels     (map get-value stacks)]
    (display (partition x pixels))))

;;; Problem 1
(defn p01 [file x y]
  (->> file
       (slurp)
       (re-seq #"\d")
       (map first)
       (map num-map)
       (solve-for-min-zeros x y)))

;;; Problem 2
(defn p02 [file x y]
  (->> file
       (slurp)
       (re-seq #"\d")
       (map first)
       (map num-map)
       (decode-image x y)))
