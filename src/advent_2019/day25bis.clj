(ns advent-2019.day25bis
  (:require [advent-2019.intcode :as ic]
            [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/25

;;; Improved version of day 23, updated for changes to the intcode module.

;; A ref used to hold the output from the program. Set as a global (for now) so
;; that consume-output can alter it and other fns can access it.
(def ^:private buffer (ref nil))

;; Output call-back for this program. Treats output as a stream of ASCII codes
;; and puts them into "lines" (vectors), starting a new line when a code 10
;; (\n) is read.
(defn- consume-output [m out]
  (cond
    (= out 10) (dosync (alter buffer conj []))
    :else      (dosync (alter buffer
                              update (dec (count @buffer))
                              conj (if (< out 256) (char out) out))))
  m)

;; This was a debugging fn in a previous day, but use it here to show the
;; program output (including the problem answer).
(defn- print-buffer []
  (println (str/join "\n" (map str/join @buffer))))

;; Encode a line of text as ASCII int values and stuff them into the input
;; buffer. Add a 10 on the end for the newline, to avoid having to include it
;; on every line.
(defn- send-line [m line]
  (apply ic/add-input (cons m (concat (map int (seq line)) (list 10)))))

;; Run the text adventure. Bleh.
(defn- play-the-game [m]
  (loop [m m]
    (print-buffer)
    (dosync (ref-set buffer [[]]))
    (let [input (read-line)]
      (recur (ic/execute (send-line m input))))))

;;; Problem 1
(defn p01 [file]
  (do
    (dosync (ref-set buffer [[]]))
    (-> file
        ic/read-opcodes
        (ic/initialize-machine :output consume-output)
        ic/execute
        play-the-game)))

;;; No problem 2 on day 25.
