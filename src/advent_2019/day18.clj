(ns advent-2019.day18
  (:require [advent-2019.heap :as h]
            [clojure.java.io :as io]
            [clojure.set :as sets]))

;;; https://adventofcode.com/2019/day/18

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Movement vectors for the path-searching.
(def ^:private moves [[1 0] [-1 0] [0 -1] [0 1]])

;; Derive valid moves from pos on g. Assumes that any blocks are "#"
;; characters.
(defn- valid-moves [g pos]
  (reduce (fn [l m]
            (let [p' (mapv + pos m)]
              (if (= \# (get-in g p')) l (cons p' l))))
          () moves))

;; Do a breadth-first search in grid g from the (X,Y) given, generating a map
;; of keys, their distance from (X,Y), and a set of any doors along the way.
(defn- breadth-first [g [y-in x-in]]
  (let [start (list (list y-in x-in 0 #{}))]
    (loop [[item & queue] start, seen #{}, keym {}]
      (cond
        (nil? item) keym
        :else
        (let [[y x t doors] item]
          (cond
            (seen [y x]) (recur queue seen keym)
            :else
            (recur (reduce (fn [l [dy dx]]
                             (let [c      (get-in g [dy dx])
                                   doors' (if (Character/isUpperCase c)
                                            (conj doors
                                                  (Character/toLowerCase c))
                                            doors)]
                               (concat l (list (list dy dx (inc t) doors')))))
                           queue (valid-moves g [y x]))
                   (conj seen [y x])
                   (if (and (not= [y x] [y-in x-in])
                            (Character/isLowerCase (get-in g [y x])))
                     (assoc keym (get-in g [y x]) (list t doors))
                     keym))))))))

;; Generate new search nodes for the pseudo-Dijkstra implementation in the
;; find-shortest-steps fn, below. "bots" is a set that holds all the "runners"
;; for the search (just "@" for part 1, or the 4 bots for part 2). "graph" is
;; the graph built for all runners or keys, using breadth-first above. "ks" is
;; the set of keys currently held, "dist" is the base distance at this point in
;; the search, and "c" is the key into "graph" for getting distance tables.
(defn- search-nodes [bots graph ks dist c]
  (reduce (fn [l k]
            (let [[d doors] (get-in graph [c k])]
              (if (and (not (ks k))
                       (empty?
                        (sets/difference doors ks)))
                (cons {:key (+ d dist)
                       :data (list (conj (disj bots c) k) (conj ks k))} l)
                l)))
          () (keys (graph c))))

;; This is a modified heap-based Dijkstra search. It takes a set of "runners"
;; (which is just "@" for part 1, or the four robots for part 2) and the parsed
;; input. From this it determines the keys, doors, etc. for the field and runs
;; the search until the head of the heap holds a solution that has all the
;; keys.
(defn- find-shortest-steps [bots g]
  (let [setup (reduce (fn [m [c y x]]
                        (assoc m c [y x]))
                      {} (for [y (range (count g)), x (range (count (first g)))
                               :when
                               (or (Character/isLowerCase (get-in g [y x]))
                                   (bots (get-in g [y x])))]
                           (list (get-in g [y x]) y x)))
        keyc  (- (count (keys setup)) (count bots))
        graph (reduce (fn [m k]
                        (assoc m k (breadth-first g (setup k))))
                      {} (keys setup))
        heap  (h/min-heap (list {:key 0, :data (list bots #{})}))]
    (loop [heap heap, dists {}]
      (cond
        (zero? (h/size heap)) "failed"
        :else
        (let [top       (h/root heap)
              heap'     (h/delete heap)
              dist      (:key top)
              node      (:data top)
              [bots ks] node]
          (cond
            (contains? dists node) (recur heap' dists)
            (= keyc (count ks))    dist
            :else
            (recur (reduce h/insert
                           heap'
                           (apply concat
                                  (map #(search-nodes bots graph ks dist %)
                                       bots)))
                   (assoc dists node dist))))))))

;; For modifying the field for part 2, this locates a given character in the
;; field. This is used to find the "@" so it can be transformed.
(defn- find-char [g c]
  (first (for [y (range (count g)), x (range (count (first g)))
               :when (= c (get-in g [y x]))]
           (list y x))))

;; Transform the field map as specified for part 2.
(defn- mod-map [g]
  (let [loc (find-char g \@)
        g'  (reduce (fn [m [c y x]]
                      (assoc-in m [y x] (char c)))
                    g (map cons
                           (iterate inc (int \0))
                           (list (map + loc [-1 -1])
                                 (map + loc [-1 1])
                                 (map + loc [1 -1])
                                 (map + loc [1 1]))))]
    (reduce (fn [m [y x]]
              (assoc-in m [y x] \#))
            g' (cons loc (valid-moves g loc)))))

;;; Problem 1
(defn p01 [file]
  (->> file
       (read-lines)
       (mapv #(vec (seq %)))
       (find-shortest-steps #{\@})))

;;; Problem 2
(defn p02 [file]
  (->> file
       (read-lines)
       (mapv #(vec (seq %)))
       (mod-map)
       (find-shortest-steps #{\0 \1 \2 \3})))
