(ns advent-2019.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/24

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Convert [y x] to a position in 0..24.
(defn- getpos [y x] (+ x (* y 5)))

;; Count the number of bugs adjacent to the given location.
(defn- adjacent [b y x]
  (apply + (for [move [[-1 0] [1 0] [0 -1] [0 1]]]
             (let [[dy dx] (map + [y x] move)
                   pos     (getpos dy dx)]
               (cond
                 (or (neg? dx)
                     (> dx 4)
                     (neg? dy)
                     (> dy 4)) 0
                 :else         (b pos))))))

;; "Advance" the board one iteration.
(defn- advance-board [board]
  (vec (for [y (range 5), x (range 5)]
         (let [pos (getpos y x)
               adj (adjacent board y x)
               at  (board pos)]
           (cond
             (and (zero? at)
                  (< 0 adj 3))  1
             (and (= at 1)
                  (not= adj 1)) 0
             :else              at)))))

;; Find the first board configuration that repeats.
(defn- find-repeating [board]
  (loop [board board, seen #{}]
    (cond
      (seen board) board
      :else        (recur (advance-board board) (conj seen board)))))

;; Vector of powers of 2 from 0 to 24, for calculating the "biodiversity" of
;; the board that appears twice.
(def ^:private pow2
  (reduce (fn [v p]
            (conj v (* 2 (v (dec p)))))
          [1] (range 1 25)))

;; Calculate the "biodiversity" of the board by adding together the powers of
;; 2 for all the cells that are set.
(defn- calc-diversity [board]
  (apply + (map * board pow2)))

;; Part 2 is hella different...

;; Check the [y x] position to see if it is influenced by the outer grid (b).
(defn- check-outer [b y x]
  (if (nil? b)
    0
    (+ (cond
         (zero? y) (b 7)
         (= y 4)   (b 17)
         :else     0)
       (cond
         (zero? x) (b 11)
         (= x 4)   (b 13)
         :else     0))))

;; Check the [y x] position to see if it is influenced by the inner grid (b).
(defn- check-inner [b y x]
  (if (nil? b)
    0
    (+ (cond
         (and (= y 2)
              (= x 1)) (apply + (map b (list 0 5 10 15 20)))
         (and (= y 2)
              (= x 3)) (apply + (map b (list 4 9 14 19 24)))
         :else         0)
       (cond
         (and (= y 1)
              (= x 2)) (apply + (map b (range 5)))
         (and (= y 3)
              (= x 2)) (apply + (map b (range 20 25)))
         :else         0))))

;; Calculate the adjacency score for [y x] on board b. Do this by doing the
;; regular calculation, then add any influence from the inner or outer boards.
(defn- adjacent-rec [board y x outer inner]
  (let [base (adjacent board y x)]
    (+ base (check-outer outer y x) (check-inner inner y x))))

;; Advance the board at the given level of the stack.
(defn- advance-at [stack level]
  (let [board (stack level)]
    (vec (for [y (range 5), x (range 5)]
           (let [pos (getpos y x)
                 at  (board pos)
                 adj (if (= pos 12)
                       0
                       (adjacent-rec board y x
                                     (get stack (dec level) nil)
                                     (get stack (inc level) nil)))]
             (cond
               (and (zero? at)
                    (< 0 adj 3))  1
               (and (= at 1)
                    (not= adj 1)) 0
               :else              at))))))

;; Advance all the boards in a semi-recursive manner, with each level being
;; influenced by (and influencing) the outer and inner levels it is linked to.
(defn- advance-recursive [cycles board]
  (let [stack {0 board}]
    (loop [stack stack, cycle 0]
      (cond
        (= cycle cycles) stack
        :else
        (let [lvls   (keys stack)
              minlvl (apply min lvls)
              maxlvl (apply max lvls)
              stack  (assoc stack
                            (dec minlvl) (vec (repeat 25 0))
                            (inc maxlvl) (vec (repeat 25 0)))]
          (recur (reduce (fn [stk lvl]
                           (assoc stk lvl (advance-at stack lvl)))
                         stack (keys stack))
                 (inc cycle)))))))

;; Count the bugs on all levels.
(defn- count-bugs [stack]
  (apply + (map #(reduce + %) (vals stack))))

;;; Problem 1
(defn p01 [file]
  (->> file
       read-lines
       str/join
       seq
       (mapv {\. 0, \# 1})
       find-repeating
       calc-diversity))

;;; Problem 2
(defn p02 [file & [steps]]
  (->> file
       read-lines
       str/join
       seq
       (mapv {\. 0, \# 1})
       (advance-recursive (or steps 200))
       (count-bugs)))
