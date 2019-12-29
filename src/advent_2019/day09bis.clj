(ns advent-2019.day09bis
  (:require [advent-2019.intcode :as ic]))

;;; https://adventofcode.com/2019/day/9

;;; "Upgraded" version of day09.clj. Uses the intcode machine from the separate
;;; module.

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
      (ic/initialize-machine :input (list 2))
      ic/execute
      :output))
