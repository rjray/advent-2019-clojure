(ns advent-2019.day06
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]))

;;; https://adventofcode.com/2019/day/6

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Take a stream of lines and transform them into pairs of ("A" "B"), where
;; B directly orbits A.
(defn- convert-lines [lines]
  (map #(re-seq #"\w+" %) lines))

;; Build a rudimentary graph, where the key is the orbiting body and its value
;; is the body it orbits.
(defn- build-graph [pairs]
  (reduce (fn [m p]
            (assoc m (last p) (first p)))
          {} pairs))

;; Count the total edges in the rudimentary graph. Needed for problem 1.
(defn- edge-count [m e]
  (loop [e e, count 0]
    (cond
      (= e "COM") count
      :else       (recur (m e) (inc count)))))

;; Build a full graph. We need the rudimentary graph to know what bodies "YOU"
;; and "SAN" are orbiting, and we need a regular graph that shows all the edges
;; (undirected).
(defn- build-full-graph [pairs]
  (let [orbits (build-graph pairs)
        graph  (reduce (fn [g [a b]]
                         (assoc g
                                a (cons b (get g a ()))
                                b (cons a (get g b ()))))
                       {} pairs)]
    {:orbits orbits, :graph graph}))

;; Calculate the shortest path from the body that "from" is orbiting, to the
;; body that "to" is orbiting. Uses the naive Dijkstra algorithm.
(defn- shortest-path [from to g]
  (let [{graph :graph
         orbits :orbits} g
        start            (orbits from)
        finish           (orbits to)
        all-nodes        (set (keys graph))
        node-count       (count all-nodes)
        initial-dists    (reduce (fn [m k]
                                   (assoc m k node-count))
                                 {} all-nodes)]
    (loop [unvisited all-nodes
           dists     (assoc initial-dists start 0)
           current   start]
      (cond
        (not (unvisited finish)) (dists finish)
        :else
        (let [cur-dist   (dists current)
              candidates (sets/intersection unvisited (set (graph current)))
              new-dists  (reduce (fn [ds node]
                                   (assoc ds
                                          node (min (ds node) (inc cur-dist))))
                                 dists candidates)
              new-unvis  (disj unvisited current)
              new-cur    (first (sort #(compare (new-dists %1)
                                                (new-dists %2)) new-unvis))]
          (recur new-unvis new-dists new-cur))))))

;;; Problem 1
(defn p01 [file]
  (as-> file $
    (read-lines $)
    (convert-lines $)
    (build-graph $)
    (map #(edge-count $ %) (keys $))
    (apply + $)))

;;; Problem 2
(defn p02 [file]
  (->> file
       (read-lines)
       (convert-lines)
       (build-full-graph)
       (shortest-path "YOU" "SAN")))
