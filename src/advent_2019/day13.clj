(ns advent-2019.day13
  (:require [advent-2019.intcode :as ic]
            [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/13

;; Read the given file as a stream of comma-separated data values. Return a
;; vector of the numerical values.
(defn- read-opcodes [file]
  (->> file
       (slurp)
       (re-seq #"-?\d+")
       (map #(Long/parseLong %))))

(def ^:private output (ref ()))
(def ^:private screen (ref {}))
(def ^:private score (ref 0))
(def ^:private ball-x (ref 0))
(def ^:private paddle-x (ref 0))

(def ^:private tiles [" " "#" "*" "-" "."])

(defn- consume-output [state out]
  (let [cur-output @output
        cur-output (cons out cur-output)]
    (if (= 3 (count cur-output))
      (do
        (if (and (= 4 (first cur-output)) (pos? (last cur-output)))
          (dosync (ref-set ball-x (last cur-output))))
        (if (and (= 3 (first cur-output)) (pos? (last cur-output)))
          (dosync (ref-set paddle-x (last cur-output))))
        (let [[id y x] cur-output]
          (if (= x -1)
            (dosync (ref-set score id))
            (dosync (alter screen assoc (list x y) id))))
        (dosync (ref-set output ())))
      (dosync (ref-set output cur-output)))
    state))

(defn- show-screen []
  (let [grid   @screen
        points (keys grid)
        max-x  (apply max (map first points))
        max-y  (apply max (map last points))
        field  (vec (repeat (inc max-y) (vec (repeat (inc max-x) ""))))
        field  (reduce (fn [f p]
                         (let [[x y] p]
                           (assoc-in f [y x] (tiles (grid p)))))
                       field points)]
    (println (str/join "\n" (map #(apply str %) field)))))

(defn- choose-direction [] (compare @ball-x @paddle-x))

(defn- play-game [machine]
  (loop [m machine]
    (cond
      (:halted m)  @score
      (:blocked m) (recur (ic/add-input m (choose-direction)))
      :else        (recur (ic/execute m)))))

;;; Problem 1
(defn p01 [file]
  (let [final-state (as-> file $
                      (read-opcodes $)
                      (ic/initialize-machine $ :output consume-output)
                      (ic/execute $))]
    (do
      (show-screen)
      (as-> @screen $
        (vals $)
        (frequencies $)
        ($ 2)))))

;;; Problem 2
(defn p02 [file]
  (let [initial-state (as-> file $
                        (read-opcodes $)
                        (cons 2 (rest $))
                        (ic/initialize-machine $ :output consume-output))]
    (play-game initial-state)))
