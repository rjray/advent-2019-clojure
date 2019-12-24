(ns advent-2019.day20
  (:require [advent-2019.heap :as h]
            [clojure.java.io :as io]
            [clojure.set :as sets]))

;;; https://adventofcode.com/2019/day/20

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Movement vectors for the path-searching.
(def ^:private moves [[1 0] [-1 0] [0 -1] [0 1]])

;; Derive valid moves from pos on g. Assumes that any blocks are "#"
;; characters.
(defn- valid-moves [f pos]
  (reduce (fn [l m]
            (let [p' (mapv + pos m)]
              (if (= \. (get-in f p')) (cons p' l) l)))
          () moves))

;; Version of valid-moves just used for checking for dead-ends.
(defn- valid-moves-dead-end [f pos]
  (reduce (fn [l m]
            (let [p' (mapv + pos m)]
              (if (= \# (get-in f p')) l (cons p' l))))
          () moves))

;; Predicate to see if the given point is surrounded by walls on 3 of the 4
;; sides.
(defn- dead-end? [f p]
  (= 1 (count (valid-moves-dead-end f p))))

;; Modify the field so that all "dead-end" corridors are plugged.
(defn- plug-dead-ends [field]
  (let [ys (count field), xs (count (first field))]
    (loop [field field]
      (let [field' (reduce (fn [m p]
                             (assoc-in m p \#))
                           field
                           (for [y (range 2 (- ys 2)), x (range 2 (- xs 2))
                                 :when (and (= \. (get-in field [y x]))
                                            (dead-end? field [y x]))]
                             [y x]))]
        (cond
          (= field field') field
          :else            (recur field'))))))

;; Find a portal around a given position, if there is one.
(defn- find-portal [field pos]
  (let [around (zipmap (map #(mapv + pos %) moves)
                       (list :down :up :left :right))
        loc    (first (filter #(Character/isUpperCase (get-in field %))
                              (keys around)))]
    (when loc
      (case (around loc)
        :up    (str (get-in field (mapv + loc [-1 0])) (get-in field loc))
        :down  (str (get-in field loc) (get-in field (mapv + loc [1 0])))
        :left  (str (get-in field (mapv + loc [0 -1])) (get-in field loc))
        :right (str (get-in field loc) (get-in field (mapv + loc [0 1])))))))

;; Get the portals in the field.
(defn- get-portals [field]
  (let [ys      (count field)
        xs      (count (first field))
        gridmap (into {} (for [y (range ys), x (range xs)
                               :when (= \. (get-in field [y x]))]
                           [[y x] \.]))]
    (reduce (fn [m p]
              (let [portal (find-portal field p)]
                (if portal (assoc m p portal) m)))
            {} (keys gridmap))))

;; Create an end-to-end map of the portals (other than AA and ZZ), so that it
;; is easier to move between them.
(defn- portals-map [portals]
  (let [grouping (group-by last (reduce (fn [l k]
                                          (cons (list k (portals k)) l))
                                        () (keys portals)))]
    (reduce (fn [m [a b]]
              (assoc m (first a) (first b) (first b) (first a)))
            {} (vals grouping))))

;; Find the valid moves for the given position.
(defn- find-moves [field portals seen pos dist]
  (let [basic-moves  (valid-moves field pos)
        moves+portal (if (portals pos)
                       (cons (portals pos) basic-moves)
                       basic-moves)
        all-moves    (remove seen moves+portal)]
    (map #(hash-map :key (inc dist) :data (list % (conj seen %))) all-moves)))

;; Get the shortest path from AA to ZZ.
(defn- get-shortest-path [field]
  (let [portals      (get-portals field)
        {start "AA",
         end   "ZZ"} (sets/map-invert portals)
        portals      (portals-map (dissoc portals start end))
        heap         (h/min-heap (list {:key 0, :data (list start #{start})}))]
    (loop [heap heap]
      (cond
        (zero? (h/size heap)) "failed"
        :else
        (let [top          (h/root heap)
              heap'        (h/delete heap)
              {dist :key
               node :data} top
              [pos seen]   node]
          (if (= pos end)
            dist
            (recur (reduce h/insert
                           heap'
                           (find-moves field portals seen pos dist)))))))))

;; Part 2 requires a different approach entirely. Could have done part 1 this
;; way, too, which would have made part 2 a lot easier...

;; Get the portals in a different way than in part 1.
(defn- get-portals' [field]
  (let [ys      (count field)
        xs      (count (first field))
        gridmap (into {} (for [y (range ys), x (range xs)
                               :when (= \. (get-in field [y x]))]
                           [[y x] \.]))]
    (reduce (fn [m p]
              (let [portal (find-portal field p)]
                (if portal (assoc m portal (cons p (get m portal ()))) m)))
            {} (keys gridmap))))

;; Same idea as for part 1, but "portals" has a different structure.
(defn- portals-map' [portals]
  (reduce (fn [m [a b]]
            (assoc m a b b a))
          {} (vals (dissoc portals "AA" "ZZ"))))

;; Derive valid moves from pos on g. Assumes that any blocks are "#"
;; characters. Allows "movement" into upper-case letters (as portals).
(defn- valid-moves' [f pos]
  (reduce (fn [l m]
            (let [p' (mapv + pos m)
                  c  (get-in f p')]
              (if (or (= \. c)
                      (Character/isUpperCase c)) (cons p' l) l)))
          () moves))

;; Calculate a portal move into a new level.
(defn- portal-move [field w h portmap old pos lvl]
  (when (portmap old)
    (let [[ny nx] pos
          outer   (or (= nx 1) (= nx (- w 2))
                      (= ny 1) (= ny (- h 2)))
          newlvl  (if outer (dec lvl) (inc lvl))
          newpos  (portmap old)]
      (when (>= newlvl 0) (list newpos newlvl)))))

;; Find the valid moves for the given position. In this case, portals also
;; change the level of the maze.
(defn- find-moves' [field w h portmap seen pos lvl]
  (reduce (fn [l p]
            (let [c        (get-in field p)
                  portal   (Character/isUpperCase c)
                  new-move (if portal
                             (portal-move field w h portmap pos p lvl)
                             (list p lvl))]
              (if (and (not= nil new-move)
                       (not (seen new-move))) (cons new-move l) l)))
          () (valid-moves' field pos)))

;; Get the shortest path from AA to ZZ, with the added stipulation that moving
;; through portals changes the level of the maze.
(defn- get-shortest-path' [field]
  (let [h       (count field)
        w       (count (first field))
        portals (get-portals' field)
        portmap (portals-map' portals)
        start   (first (portals "AA"))
        end     (first (portals "ZZ"))]
    (loop [queue (list (list start 0 0)), seen #{(list start 0)}]
      (if (empty? queue)
        "failed"
        (let [[[pos lvl dist] & queue'] queue]
          (cond
            (and (= pos end)
                 (zero? 0))  dist
            :else
            (let [new-moves (find-moves' field w h portmap seen pos lvl)]
              (recur (concat queue' (map #(concat % (list (inc dist)))
                                         new-moves))
                     (reduce conj seen new-moves)))))))))

;;; Problem 1
(defn p01 [file]
  (->> file
       (read-lines)
       (mapv #(vec (seq %)))
       (plug-dead-ends)
       (get-shortest-path)))

;;; Problem 2
(defn p02 [file]
  (->> file
       (read-lines)
       (mapv #(vec (seq %)))
       (plug-dead-ends)
       (get-shortest-path')))
