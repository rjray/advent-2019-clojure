(ns advent-2019.day13bis
  (:require [advent-2019.intcode :as ic]
            [clojure.string :as str]))

;;; Clean-up and hopefully improvements.

;; Read the given file as a stream of comma-separated data values. Return a
;; vector of the numerical values.
(defn- read-opcodes [file]
  (->> file
       (slurp)
       (re-seq #"-?\d+")
       (map #(Long/parseLong %))))

;; The complete game state in a ref that can be persisted throughout the run.
(def ^:private
  game-state (ref {:output (), :screen {}, :score 0, :ball-x 0, :paddle-x 0 }))

;; Output callback for the intcode machine. This consumes the input (one element
;; at a time) and when there are three acts on it as needed.
(defn- consume-output [state out]
  (let [cur-output (:output @game-state)
        cur-output (cons out cur-output)]
    (if (= 3 (count cur-output))
      (let [[id y x] cur-output]
        (if (and (= 4 id) (pos? x))
          (dosync (alter game-state assoc :ball-x x)))
        (if (and (= 3 id) (pos? x))
          (dosync (alter game-state assoc :paddle-x x)))
        (if (= x -1)
          (dosync (alter game-state assoc :score id))
          (dosync (alter game-state assoc-in [:screen (list x y)] id)))
        (dosync (alter game-state assoc :output ())))
      (dosync (alter game-state assoc :output cur-output)))
    state))

;; Vector of the characters used to display the current state of the screen.
(def ^:private tiles [" " "#" "*" "-" "."])

;; Only used for debugging-- dump the current state of the screen.
(defn- show-screen []
  (let [grid   (:screen @game-state)
        points (keys grid)
        max-x  (apply max (map first points))
        max-y  (apply max (map last points))
        field  (vec (repeat (inc max-y) (vec (repeat (inc max-x) ""))))
        field  (reduce (fn [f p]
                         (let [[x y] p]
                           (assoc-in f [y x] (tiles (grid p)))))
                       field points)]
    (println (str/join "\n" (map #(str/join %) field)))))

;; Choose a direction to move the paddle in, based on the current relative X
;; values of the ball and the paddle.
(defn- choose-direction []
  (compare (:ball-x @game-state) (:paddle-x @game-state)))

;; Play the game. Given an initialized machine, loop until it halts and report
;; the final score. Each iteration, if it is blocked on input then set the
;; input based on the direction the paddle should be moved in.
(defn- play-game [machine]
  (loop [m machine]
    (cond
      (:halted m)  (:score @game-state)
      (:blocked m) (recur (ic/add-input m (choose-direction)))
      :else        (recur (ic/execute m)))))

;;; Problem 1
(defn p01 [file]
  (let [final-state (as-> file $
                      (read-opcodes $)
                      (ic/initialize-machine $ :output consume-output)
                      (ic/execute $))]
    (show-screen)
    (as-> @screen $
      (vals $)
      (frequencies $)
      ($ 2))))

;;; Problem 2
(defn p02 [file]
  (let [initial-state (as-> file $
                        (read-opcodes $)
                        (cons 2 (rest $))
                        (ic/initialize-machine $ :output consume-output))]
    (play-game initial-state)))
