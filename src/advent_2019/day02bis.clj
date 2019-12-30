(ns advent-2019.day02bis
  (:require [advent-2019.intcode :as ic]))

;;; https://adventofcode.com/2019/day/2

;;; "Upgraded" version of day02.clj. Uses the intcode machine from the separate
;;; module.

;; Get the resulting value from the "program" given, using the two values as
;; patch-values for memory locations 1 and 2 (respectively).
(defn- find-value [machine v1 v2]
  (-> machine
      (ic/patch 1 v1 2 v2)
      ic/execute
      :memory
      (get 0)))

;; Find the v1/v2 pair that results in the given program returning a value
;; equal to v. For now, brute-force it over the range that v1 and v2 can
;; take (0-99).
(defn- find-values [machine val]
  (loop [v1 0, v2 0]
    (let [result (find-value machine v1 v2)]
      (cond
        (= result val) (+ (* 100 v1) v2)
        (< v2 99)      (recur v1 (inc v2))
        (< v1 99)      (recur (inc v1) 0)
        :else          "No solution"))))

;;; Problem 1
;;; Run the opcodes in "file", after patching the stream to have 12 in position
;;; 1 and 2 in position 2. Return the value that remains in position 0 after
;;; the "program" halts.
(defn p01 [file]
  (-> file
      ic/read-opcodes
      ic/initialize-machine
      (find-value 12 2)))

;;; Problem 2
;;; This time, try to figure out what pair of patch-values for pos 1 and 2 will
;;; result in a final value equal to val.
(defn p02 [file val]
  (-> file
      ic/read-opcodes
      ic/initialize-machine
      (find-values val)))
