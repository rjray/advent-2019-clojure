(ns advent-2019.day07bis
  (:require [advent-2019.intcode :as ic]
            [clojure.math.combinatorics :as comb]))

;;; https://adventofcode.com/2019/day/7

;;; "Upgraded" version of day07.clj. Uses the intcode machine from the separate
;;; module.

;; Cycle the vector of amps by updating the amp at idx with new-state (while
;; dropping the top-most output value) and adding said output value to the
;; input of the next amp in the vector (with wrap-around).
(defn- cycle-amps [amps new-state idx idx']
  (assoc amps
         idx (assoc (ic/drop-output new-state) :blocked false)
         idx' (ic/add-input (amps idx') (ic/get-output new-state))))

;; For problem 1, run the given permutation on the "program" given, in which
;; each amp runs just once and feeds its output to the next one in line. When
;; the last has run, its output is the value for problem 1.
(defn- run-permutation-normal [program perm]
  (loop [[p & ps] perm, input (list 0)]
    (cond
      (nil? p) (cons (first input) perm)
      :else
      (let [machine (ic/initialize-machine program
                                           :input (list p (first input)))
            output  (:output (ic/execute machine))]
        (recur ps output)))))

;; For problem 2, run the given permutations on the "program", in which the
;; amps keep feeding into each other until the last amp has halted. At that
;; point, it's output value is the value for problem 2.
(defn- run-permutation-with-feedback [program perm]
  (let [amps (vec (map #(ic/initialize-machine program :input (list %)) perm))
        amps (update-in amps [0] ic/add-input 0)]
    (loop [amps amps, idx 0]
      (let [new-state (ic/execute (amps idx))]
        (cond
          (:halted new-state)  (if (= (inc idx) (count amps))
                                 (cons (ic/get-output new-state) perm)
                                 (let [idx' (mod (inc idx) 5)]
                                   (recur (cycle-amps amps new-state idx idx')
                                          idx')))
          (:blocked new-state) (let [idx' (mod (inc idx) 5)]
                                 (recur (cycle-amps amps new-state idx idx')
                                        idx')))))))

;; Run all the permutations for the "program" given, based on the vector of
;; settings. When done, return the largest result value from all the
;; permutations. Use the given function to choose how the amps are run.
(defn- run-all-permutations [program fn settings]
  (let [perms (comb/permutations settings)]
    (loop [[p & ps] perms, results ()]
      (cond
        (nil? p) (first (sort #(compare (first %2) (first %1)) results))
        :else    (recur ps (cons (fn program p) results))))))

;;; Problem 1
(defn p01 [file]
  (-> file
      ic/read-opcodes
      (run-all-permutations run-permutation-normal (vec (range 5)))))

;;; Problem 2
(defn p02 [file]
  (-> file
      ic/read-opcodes
      (run-all-permutations run-permutation-with-feedback (vec (range 5 10)))))
