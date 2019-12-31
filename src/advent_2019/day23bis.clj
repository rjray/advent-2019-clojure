(ns advent-2019.day23bis
  (:require [advent-2019.intcode :as ic]
            [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/23

;;; Improved version of day 23, updated for changes to the intcode module.

;; Buffer output into this ref, using the next defn as a call-back in the
;; intcode machines that get created.
(def ^:private buffer (ref ()))

(defn- buffer-output [state out]
  (dosync (alter buffer concat (list out)))
  state)

;; Create num intcode machines from the initial "program", and seed each one
;; with its index as a single input.
(defn- create-machines [code num]
  (mapv #(ic/add-input code %) (range num)))

;; Stuff a list of input into the given machine.
(defn- stuff-input [machine input]
  (apply ic/add-input (cons machine input)))

;; Part 1-- run the machines in a loop until the first "packet" headed for
;; address 255 is detected.
(defn- run-in-loop [machines]
  (loop [machines machines]
    (let [m'  (mapv ic/execute machines)
          out @buffer]
      (dosync (ref-set buffer ()))
      (if (zero? (rem (count out) 3))
        (let [messages (partition 3 out)
              found    (first (filter #(= 255 (first %)) messages))]
          (if (nil? found)
            (let [mq (reduce (fn [m [to x y]]
                               (assoc m to (concat (get m to ()) (list x y))))
                             {} messages)]
              (recur (mapv #(stuff-input (m' %) (get mq % (list -1)))
                           (range (count machines)))))
            (last found)))
        "output error!"))))

;; Check for the "network" to be idle due to all machines having no pending
;; messages in their input queues.
(defn- network-idle? [m]
  (= (count m)
     (count (filter #(= (list -1) (:input %)) m))))

;; Tell if the two most-recently-sent Y values in the NAT traffic stream are
;; identical.
(defn- repeat? [s] (= (first s) (first (rest s))))

;; A slightly-modified "stuff-input" that deals with a vector of lists.
(defn- stuff-inputs-nat [m v]
  (stuff-input m (mapcat rest v)))

;; "Send" the NAT values to machine 0.
(defn- apply-nat [mv nat]
  (update-in mv [0] stuff-input nat))

;; Part 2-- largely the same as run-in-loop, but it keeps going until the NAT
;; sends the same Y value twice in a row.
(defn- run-in-nat-loop [machines]
  (loop [machines machines, nat (), traffic ()]
    (cond
      (and (>= (count traffic) 2)
           (repeat? traffic))        (first traffic)
      (and (seq nat)
           (network-idle? machines)) (recur (apply-nat machines nat)
                                            ()
                                            (cons (last nat) traffic))
      :else
      (let [m'  (mapv ic/execute machines)
            out @buffer]
        (dosync (ref-set buffer ()))
        (if (zero? (rem (count out) 3))
          (let [messages (group-by first (partition 3 out))]
            (recur (mapv #(stuff-inputs-nat (m' %)
                                            (get messages % (list [-1 -1])))
                         (range (count machines)))
                   (if (contains? messages 255)
                     (rest (last (messages 255))) nat)
                   traffic))
          "output error!")))))

;;; Problem 1
(defn p01 [file]
  (-> file
      ic/read-opcodes
      (ic/initialize-machine :output buffer-output)
      (create-machines 50)
      run-in-loop))

;;; Problem 2
(defn p02 [file]
  (-> file
      ic/read-opcodes
      (ic/initialize-machine :output buffer-output)
      (create-machines 50)
      run-in-nat-loop))
