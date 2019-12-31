(ns advent-2019.day05bis
  (:require [advent-2019.intcode :as ic]))

;;; https://adventofcode.com/2019/day/5

;;; "Upgraded" version of day05.clj. Uses the intcode machine from the separate
;;; module. Turns out, that's all that is needed for this one.

;;; Problem 1
(defn p01 [file]
  (-> file
      ic/read-opcodes
      (ic/initialize-machine :input (list 1))
      ic/execute
      :output))

;;; Problem 2
(defn p02 [file]
  (-> file
      ic/read-opcodes
      (ic/initialize-machine :input (list 5))
      ic/execute
      :output))
