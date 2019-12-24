(ns advent-2019.day16
  (:require [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/16

;; Use these to turn number-strings or number-chars into the numbers quickly.
(def ^:private num-chr {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})
(def ^:private num-str {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4
                        "5" 5 "6" 6 "7" 7 "8" 8 "9" 9})

;; Read the given file as a long string of digits:
(defn- read-signal [file]
  (->> file
       (slurp)
       (re-seq #"\d")
       (map num-str)))

;; This is the repeating pattern used in part 1.
(def ^:private base-pattern [0 1 0 -1])

;; Calculate one line of the phase. Turns base-pattern into an expanded,
;; infinitely-repeating lazy sequence, then uses that along with map and
;; reduce to sum the multiplications of the input and the lazy-seq. Return
;; only the last digit.
(defn- calc-op [op input]
  (let [pattern (mapcat #(repeat op %) base-pattern)
        stream  (cycle pattern)]
    (num-chr (last (str (reduce +' (map *
                                        input (rest stream))))))))

;; Run a single phase of the FFT process on the input in "signal". Use the
;; length of the input to determine how many times calc-op is called.
(defn- run-one-phase [phase signal]
  (let [ops (count signal)]
    (loop [output (), op 0]
      (cond
        (= op ops) (reverse output)
        :else      (recur (cons (calc-op (inc op) signal) output) (inc op))))))

;; Run the FFT algorithm on the input in "signal", for the number of phases
;; given in "phases".
(defn- run-fft [phases signal]
  (loop [input signal, phase 0]
    (cond
      (= phase phases) input
      :else            (recur (run-one-phase (inc phase) input) (inc phase)))))

;; Use a partial-sums algorithm to calculate a signle phase of the "extended"
;; FFT algorithm for part 2.
(defn- run-partial-sum-calc [input]
  (let [s   (reduce + input)
        len (count input)
        iv  (vec input)]
    (loop [out (), s s, i 0]
      (cond
        (= i len) (reverse out)
        :else     (recur (cons (rem (+ 10 (rem s 10)) 10) out)
                         (- s (iv i))
                         (inc i))))))

;; For part 2, run the above funtion a number of times given in "phases". Prep
;; the input by reading the first seven digits as an offset and dropping that
;; many off the front of the lazy-seq before running the calculations.
(defn- run-full-fft [phases signal]
  (let [offset (Integer/parseInt (str/join (take 7 signal)))
        input  (drop offset (apply concat (repeat 10000 signal)))]
    (loop [input input, phase 0]
      (cond
        (= phase phases) input
        :else            (recur (run-partial-sum-calc input) (inc phase))))))

;;; Problem 1
(defn p01 [file phases]
  (->> file
       (read-signal)
       (run-fft phases)
       (take 8)
       (apply str)))

;;; Problem 2
(defn p02 [file phases]
  (->> file
       (read-signal)
       (run-full-fft phases)
       (take 8)
       (apply str)))
