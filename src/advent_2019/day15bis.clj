(ns advent-2019.day15bis
  (:require [advent-2019.intcode :as ic]
            [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/15

;;; Version of day15.clj that has some more clean-up (more idiomatic code,
;;; etc.) and changes to the intcode module.

;; Movement vectors for the N-S-W-E movement calculation.
(def ^:private moves [[0 1] [0 -1] [-1 0] [1 0]])

;; Calculate the new position for a given move from pos in dir.
(defn- move-to [pos dir] (mapv + pos (moves dir)))

;; Try to move to the new position "p" in direction "d" using the current
;; state of the machine ("m"). If p is a wall, return nil. Otherwise, return
;; a list of p, the updated machine, and the result of moving to p.
(defn- attempt-move [p d m]
  (let [m'  (ic/execute (ic/add-input m d))
        res (ic/get-output m')
        m'  (ic/drop-output m')]
    (cond
      (zero? res) nil
      :else       (list p m' res))))

;; Find the full grid of non-wall squares. Like a Dijkstra shortest-path
;; search, but it doesn't stop when the "oxygen system" is found. Instead, it
;; explores the whole grid. This means the resulting grid can be used for both
;; part 1 and part 2.
(defn- find-grid [machine]
  (loop [grid     {[0 0] (list 1 0)}
         dists    {[0 0] 0}
         frontier (list (list [0 0] machine 1))
         visited  #{[0 0]}]
    (cond
      (empty? frontier) grid
      :else
      (let [[cur & front'] frontier
            [pos m type]   cur
            cur-dist       (dists pos)
            grid'          (assoc grid pos (list type cur-dist))
            choices        (filter #(not (visited (first %)))
                                   (map #(list (move-to pos %)
                                               (inc %))
                                        (range 4)))]
        (recur grid'
               (reduce (fn [m [p]]
                         (assoc m p (inc cur-dist)))
                       dists choices)
               (reduce (fn [l [p d]]
                         (let [node (attempt-move p d m)]
                           (if node (concat l (list node)) l)))
                       front' choices)
               (apply conj (cons visited (map first choices))))))))

;; For part 1: Find the shortest-path to the "oxygen system". Since this will
;; be the only grid entry whose value has 2 for the square type, this is just a
;; filter op with some steps after the filtering.
(defn- find-shortest-path [grid]
  (last (first (filter #(= 2 (first %)) (vals grid)))))

;; For part 2: Starting from the oxygen system square, determine how long it
;; takes for all square to fill with oxygen. Sort of a search, again, with it
;; again going over the whole field rather than stopping at a target. Each
;; newly-explored square gets the time associated with it that it was filled
;; at. At the end, just find the maximum over all the times.
(defn- calc-fill-time [grid]
  (let [start (first (filter #(= 2 (first (grid %))) (keys grid)))]
    (loop [frontier (list start), visited #{start}, times {start 0}]
      (cond
        (empty? frontier) (apply max (vals times))
        :else
        (let [[cur & front'] frontier
              cur-time       (inc (times cur))
              choices        (filter #(and (not (visited %))
                                           (contains? grid %))
                                     (map #(move-to cur %) (range 4)))]
          (recur (concat front' choices)
                 (apply conj (cons visited choices))
                 (reduce (fn [t p]
                           (assoc t p cur-time))
                         times choices)))))))

;;; Problem 1
(defn p01 [file]
  (-> file
      ic/read-opcodes
      ic/initialize-machine
      find-grid
      find-shortest-path))

;;; Problem 2
(defn p02 [file]
  (-> file
      ic/read-opcodes
      ic/initialize-machine
      find-grid
      calc-fill-time))
