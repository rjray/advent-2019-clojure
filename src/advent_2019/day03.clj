(ns advent-2019.day03
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.math.numeric-tower :refer [abs]]))

;;; https://adventofcode.com/2019/day/3

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; The four types of "movement"
(defn- moveU [spots x y d]
  (list x (+ y d) (reduce (fn [s i]
                            (conj s [x (+ y i 1)]))
                          spots (range d))))

(defn- moveD [spots x y d]
  (list x (- y d) (reduce (fn [s i]
                            (conj s [x (- y i 1)]))
                          spots (range d))))

(defn- moveL [spots x y d]
  (list (- x d) y (reduce (fn [s i]
                            (conj s [(- x i 1) y]))
                          spots (range d))))

(defn- moveR [spots x y d]
  (list (+ x d) y (reduce (fn [s i]
                            (conj s [(+ x i 1) y]))
                          spots (range d))))

;; Process a line by parsing it into a series of direction/distance pairs and
;; using the movement routines above. Builds a set of all the "points" covered
;; by the wire's layout.
(defn- process-line [l]
  (let [moves (re-seq #"([UDLR])(\d+)" l)]
    (loop [[move & moves] moves, x 0, y 0, spots #{}]
      (cond
        (nil? move) spots
        :else
        (let [[_ dir dist] move
              dist         (Integer/parseInt dist)]
          (cond
            (= dir "U") (let [[x' y' spots'] (moveU spots x y dist)]
                          (recur moves x' y' spots'))
            (= dir "D") (let [[x' y' spots'] (moveD spots x y dist)]
                          (recur moves x' y' spots'))
            (= dir "L") (let [[x' y' spots'] (moveL spots x y dist)]
                          (recur moves x' y' spots'))
            (= dir "R") (let [[x' y' spots'] (moveR spots x y dist)]
                          (recur moves x' y' spots'))))))))

;; Calculate the Manhattan Distance between two points:
(defn- manhattan-dist [p1 p2]
  (+ (abs (- (first p1) (first p2)))
     (abs (- (last  p1) (last  p2)))))

;;; Problem 1
(defn p01 [file]
  (->> file
       (read-lines)
       (map process-line)
       (apply sets/intersection)
       (map #(manhattan-dist % [0 0]))
       (sort)
       (first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Updated movement routines that also calculate/track the distance along the
;; wire to each point. If a wire hits a point more than once, the first
;; (shortest) distance is kept.
(defn- moveU' [spots x y l d]
  (list x (+ y d) (+ l d) (reduce (fn [s i]
                                    (assoc s [x (+ y i 1)]
                                           (get s [x (+ y i 1)] (+ l i 1))))
                                  spots (range d))))

(defn- moveD' [spots x y l d]
  (list x (- y d) (+ l d) (reduce (fn [s i]
                                    (assoc s [x (- y i 1)]
                                           (get s [x (- y i 1)] (+ l i 1))))
                                  spots (range d))))

(defn- moveL' [spots x y l d]
  (list (- x d) y (+ l d) (reduce (fn [s i]
                                    (assoc s [(- x i 1) y]
                                           (get s [(- x i 1) y] (+ l i 1))))
                                  spots (range d))))

(defn- moveR' [spots x y l d]
  (list (+ x d) y (+ l d) (reduce (fn [s i]
                                    (assoc s [(+ x i 1) y]
                                           (get s [(+ x i 1) y] (+ l i 1))))
                                  spots (range d))))

;; Updated process routine. This builds a map with the points as keys and the
;; value at each key being the distance from origin to that point.
(defn- process-line' [l]
  (let [moves (re-seq #"([UDLR])(\d+)" l)]
    (loop [[move & moves] moves, x 0, y 0, l 0, spots {}]
      (cond
        (nil? move) spots
        :else
        (let [[_ dir dist] move
              dist         (Integer/parseInt dist)]
          (cond
            (= dir "U") (let [[x' y' l' spots'] (moveU' spots x y l dist)]
                          (recur moves x' y' l' spots'))
            (= dir "D") (let [[x' y' l' spots'] (moveD' spots x y l dist)]
                          (recur moves x' y' l' spots'))
            (= dir "L") (let [[x' y' l' spots'] (moveL' spots x y l dist)]
                          (recur moves x' y' l' spots'))
            (= dir "R") (let [[x' y' l' spots'] (moveR' spots x y l dist)]
                          (recur moves x' y' l' spots'))))))))

;; Find the shortest distance by combined wire-length for the intersection
;; points.
(defn- find-by-dist [[m1 m2]]
  (let [k1   (set (keys m1))
        k2   (set (keys m2))
        hits (sets/intersection k1 k2)]
    (first (sort (map #(+ (m1 %) (m2 %)) hits)))))

;;; Problem 2
(defn- p02 [file]
  (->> file
       (read-lines)
       (map process-line')
       (find-by-dist)))
