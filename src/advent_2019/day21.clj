(ns advent-2019.day21
  (:require [advent-2019.intcode :as ic]
            [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/21

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
(defn- consume-output [m out]
  (cond
    (= out 10) (dosync (alter field conj []))
    :else      (dosync (alter field
                              update (dec (count @field))
                              conj (if (< out 256) (char out) out))))
  m)

;; This was a debugging fn in a previous day, but use it here to show the
;; program output (including the problem answer).
(defn- print-field []
  (println (str/join "\n" (map str/join @field))))

;; Encode a line of text as ASCII int values and stuff them into the input
;; buffer. Add a 10 on the end for the newline, to avoid having to include it
;; on every line.
(defn- send-line [m line]
  (reduce (fn [s c]
            (ic/add-input s c))
          m (conj (mapv int (seq line)) 10)))

;; Apply the above fn over a list of lines, returning the resulting machine
;; state.
(defn- send-springcode [m lines]
  (reduce send-line m lines))

;;; Problem 1
(defn p01 [file]
  (do
    (dosync (ref-set field [[]]))
    (as-> file $
      (read-opcodes $)
      (ic/initialize-machine $ :output consume-output)
      (send-springcode $
                       (list "OR A J"
                             "AND B J"
                             "AND C J"
                             "NOT J J"
                             "AND D J"
                             "WALK"))
      (ic/execute $)
      (print-field))))

;;; Problem 2
(defn p02 [file]
  (do
    (dosync (ref-set field [[]]))
    (as-> file $
      (read-opcodes $)
      (ic/initialize-machine $ :output consume-output)
      (send-springcode $
                       (list "OR A J"
                             "AND B J"
                             "AND C J"
                             "NOT J J"
                             "AND D J"
                             "OR E T"
                             "OR H T"
                             "AND T J"
                             "RUN"))
      (ic/execute $)
      (print-field))))
