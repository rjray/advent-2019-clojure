(ns advent-2019.day10
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [abs]]))

;;; https://adventofcode.com/2019/day/10

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Process a single line of the input. Split it into the . and # characters,
;; and use map-indexed to provide the x-coordinate for each one.
(defn- process-line [line field y]
  (let [chars (re-seq #"[.#]" line)]
    (reduce (fn [f p]
              (if (= "#" (last p))
                (conj f (list (first p) y)) f))
            field (map-indexed list chars))))

;; Create the whole field from the sequence of lines.
(defn- create-field [lines]
  (loop [[line & lines] lines, field #{}, y 0]
    (cond
      (nil? line) field
      :else       (recur lines (process-line line field y) (inc y)))))

;; Determine which quadrant (1-4) the asteroid in question is in, based on the
;; signs of the deltas.
(defn- get-quadrant [dx dy]
  (if (neg? dx)
    (if (neg? dy) 1 3)
    (if (neg? dy) 2 4)))

;; Calculate the slope, quadrant and distance for the two asteroids given.
(defn- calc-slope-and-quad [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (cond
      (zero? dx) (list (if (neg? dy) "-inf" "inf") "-" (abs dy))
      (zero? dy) (list (if (neg? dx) "-0" "0") "-" (abs dx))
      :else      (let [quad  (get-quadrant dx dy)
                       slope (/ dy dx)
                       dist  (Math/sqrt (+ (* dx dx) (* dy dy)))]
                   (list slope quad dist)))))

;; Create the map of other asteroids relative to the given one. The map will be
;; keyed by a pair of (slope quadrant), and each value will be a list of one or
;; more asteroid points and the distance from ast.
(defn- slope-map [field ast]
  (reduce (fn [f p]
            (let [[slope quad dist] (calc-slope-and-quad ast p)
                  key               (list slope quad)
                  val               (list p dist)]
              (assoc f key (cons val (get f key ())))))
          {} field))

;; Find the best point for the "station".
(defn- find-best-point [field]
  (loop [[p & f] field, smap {}]
    (cond
      (nil? p) (let [count-map   (reduce (fn [m e]
                                           (assoc m e (count (keys (smap e)))))
                                         {} (keys smap))
                     sorted-keys (sort #(compare (count-map %2) (count-map %1))
                                       (keys count-map))]
                 (list (first sorted-keys)
                       (count-map (first sorted-keys))
                       (smap (first sorted-keys))))
      :else    (let [this-map (slope-map (disj field p) p)]
                 (recur f (assoc smap p this-map))))))

;; Fire the laser on the list of asteroids indexed by the given key. Will sort
;; the list by distance and eliminate the nearest one, returning an updated
;; view and list of destroyed asteroids.
(defn- fire-on-key [v as key]
  (let [rocks (get v key)]
    (if rocks
      (let [rocks (sort #(compare (last %1) (last %2)) rocks)
            rock  (first rocks)]
        (cond
          (= 1 (count rocks)) (list (dissoc v key) (cons (first rock) as))
          :else               (list (assoc v key (rest rocks))
                                    (cons (first rock) as))))
      (list v as))))

;; Fire on all the keys in one of the quadrants, in clockwise order. Not called
;; for the 4 axis-points (those are called directly on fire-on-key).
(defn- fire-in-quad [v as quad]
  (let [keys (sort #(compare (first %1) (first %2))
                   (filter #(= (last %) quad) (keys v)))]
    (loop [[k & ks] keys, v v, as as]
      (cond
        (nil? k) (list v as)
        :else    (let [[new-v new-as] (fire-on-key v as k)]
                   (recur ks new-v new-as))))))

;; Fire the laser. Depending on the value of "quad", either fire through all of
;; the slopes in a quadrant or for the axis-points fire directly on them.
(defn- fire-laser [v as quad]
  (cond
    (re-matches #"-?inf|-?0" (str quad)) (fire-on-key v as (list quad "-"))
    :else                                (fire-in-quad v as quad)))

;; Run the laser in a clockwise circle, starting from point due "north". This
;; will be done by iterating through:
;;
;; 0. Slope == "-inf"
;; 1. Quad == 2, ordered by slope
;; 2. Slope == "0"
;; 3. Quad == 4, ordered by slope
;; 4. Slope == "inf"
;; 5. Quad == 3, ordered by slope
;; 6. Slope == "-0"
;; 7. Quad == 1, ordered by slope
(defn- run-laser [view]
  (loop [view view, asteroids (), which 0]
    (cond
      (empty? view) (reverse asteroids)
      :else
      (case which
        0 (let [[newview newasts] (fire-laser view asteroids "-inf")]
            (recur newview newasts 1))
        1 (let [[newview newasts] (fire-laser view asteroids 2)]
            (recur newview newasts 2))
        2 (let [[newview newasts] (fire-laser view asteroids "0")]
            (recur newview newasts 3))
        3 (let [[newview newasts] (fire-laser view asteroids 4)]
            (recur newview newasts 4))
        4 (let [[newview newasts] (fire-laser view asteroids "inf")]
            (recur newview newasts 5))
        5 (let [[newview newasts] (fire-laser view asteroids 3)]
            (recur newview newasts 6))
        6 (let [[newview newasts] (fire-laser view asteroids "-0")]
            (recur newview newasts 7))
        7 (let [[newview newasts] (fire-laser view asteroids 1)]
            (recur newview newasts 0))))))

;;; Problem 1
(defn p01 [file]
  (->> file
       (read-lines)
       (create-field)
       (find-best-point)
       (take 2)))

;;; Problem 2
(defn p02 [file]
  (as-> file $
    (read-lines $)
    (create-field $)
    (find-best-point $)
    (run-laser (last $))
    (nth $ 199)
    (list $ (+ (* 100 (first $)) (last $)))))
