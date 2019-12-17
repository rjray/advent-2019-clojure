(ns advent-2019.day17
  (:require [advent-2019.intcode :as ic]
            [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/17

;; Read the given file as a stream of comma-separated data values. Return a
;; sequence of the numerical values.
(defn- read-opcodes [file]
  (->> file
       (slurp)
       (re-seq #"-?\d+")
       (map #(Long/parseLong %))))

;; A ref used to hold the output from the program. Set as a global (for now) so
;; that consume-output can alter it and other fns can access it.
(def ^:private field (ref nil))

;; Output call-back for this program. Treats output as a stream of ASCII codes
;; and puts them into "lines" (vectors), starting a new line when a code 10
;; (\n) is read.
(defn- consume-output [state out]
  (cond
    (= out 10) (dosync (alter field conj []))
    :else      (dosync (alter field
                              update (dec (count @field))
                              conj (if (< out 256) (char out) out))))
  state)

;; Used just for debugging, dumps the content of @field in a readable way.
(defn- print-field []
  (println (str/join "\n" (map str/join @field))))

;; Find the scaffolding intersections within the field as displayed by the
;; "camera". (Note that as currently written this will likely fail if made
;; part of the part 2 effort.)
(defn- find-intersections []
  (let [fieldv (vec (filter #(pos? (count %)) @field))]
    (for [y (range 1 (dec (count fieldv))),
          x (range 1 (dec (count (fieldv 0))))
          :when (and (= (get-in fieldv [y x]) \#)
                     (= (get-in fieldv [(inc y) x]) \#)
                     (= (get-in fieldv [(dec y) x]) \#)
                     (= (get-in fieldv [y (inc x)]) \#)
                     (= (get-in fieldv [y (dec x)]) \#))]
      (list x y))))

;; From the coordinates of intersections, calculate the "calibration".
(defn- calc-calibration [pts]
  (reduce + (map #(apply * %) pts)))

;;; Problem 1
(defn p01 [file]
  (do
    (dosync (ref-set field [[]]))
    (as-> file $
      (read-opcodes $)
      (ic/initialize-machine $ :output consume-output)
      (ic/execute $)
      (find-intersections)
      (calc-calibration $))))

;; For part 2: cheating for now... hand-coding the total movements needed for
;; the robot:
(def ^:private movements
  (str/join (list "L,6,L,4,R,12,L,6,R,12,R,12,L,8,L,6,L,4,R,12,L,6,L,10,L,10,"
                  "L,6,L,6,R,12,R,12,L,8,L,6,L,4,R,12,L,6,L,10,L,10,L,6,L,6,"
                  "R,12,R,12,L,8,L,6,L,4,R,12,L,6,L,10,L,10,L,6")))
;; After hand-coding, that yields the following "program":
(def ^:private program
  (str/join (list "A,B,A,C,B,A,C,B,A,C\n"
                  "L,6,L,4,R,12\n"
                  "L,6,R,12,R,12,L,8\n"
                  "L,6,L,10,L,10,L,6\n"
                  "n\n")))

;; For now, just use the "program" value above to feed the machine and then run
;; it. If it doesn't send the vac-u-bot drifting into space, then the last line
;; of output in @field will be the desired answer.
(defn- visit-all [state]
  (let [state (reduce (fn [s v]
                        (ic/add-input s v))
                      state (map int (seq program)))]
    (do
      ;; When it runs, it'll "display" the field again, but once done the last
      ;; line of @field will hold the answer.
      (ic/execute state)
      (last @field))))

;;; Problem 2
(defn p02 [file]
  (do
    (dosync (ref-set field [[]]))
    (as-> file $
      (read-opcodes $)
      (vec $)
      (assoc $ 0 2)
      (ic/initialize-machine $ :output consume-output)
      (ic/execute $)
      ;; Should be blocked on input at this point
      (visit-all $))))
