(ns advent-2019.day11bis
  (:require [advent-2019.intcode :as ic]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as str]))

;;; Day 11 code with the IntCode machine completely ripped out and put into its
;;; own module/namespace.

;; Read the given file as a stream of comma-separated data values. Return a
;; vector of the numerical values.
(defn- read-opcodes [file]
  (->> file
       (slurp)
       (re-seq #"-?\d+")
       (map #(Long/parseLong %))))

;; Create a state to represent the robot at the very start
(defn- create-robot-state []
  {:direction "up", :panels {}, :location (list 0 0)})

;; Update an existing state with the given subset of keys, returning a new
;; state.
(defn- update-state [state & params]
  (apply assoc (cons state params)))

;; Table for "turning" the robot from it's current position based on the
;; specified direction.
(def ^:private turn-map
  {"up"    ["left" "right"],
   "right" ["up" "down"],
   "down"  ["right" "left"],
   "left"  ["down" "up"]})

;; Table for making a move, based on the current facing direction.
(def ^:private move-map
  {"up"    (list 0 1),
   "right" (list 1 0),
   "down"  (list 0 -1),
   "left"  (list -1 0)})

;; Turn the robot based on the current facing and the value of "turn".
(defn- turn-robot [r turn]
  (update-state r :direction (get (turn-map (:direction r)) turn)))

;; Move the robot one square, based on the current facing.
(defn- move-robot [robot]
  (let [dir (:direction robot)
        cur (:location robot)
        new (map + cur (move-map dir))]
    (update-state robot :location new)))

;; Update the robot-state for one move. Paint the current square based on the
;; value of "paint", turn the robot, and move the robot after turning.
(defn- update-robot [r paint turn]
  (let [{location :location,
         panels :panels} r
        r' (move-robot (turn-robot r turn))]
    (update-state r' :panels (assoc panels location paint))))

;; Get the panel color-value for the panel the robot is currently on.
(defn- get-panel [robot]
  (let [{location :location, panels :panels} robot]
    (get panels location 0)))

;; Run the machine until it halts or is blocked. If the key :mock is present,
;; then don't actually run but rather take the next element from the mock-list
;; and set that as the current output state.
(defn- run-machine [machine input]
  (if (:mock machine)
    (if (empty? (:mock machine))
      (assoc machine :halted true)
      (assoc machine
             :output (first (:mock machine))
             :mock (rest (:mock machine))))
    (ic/execute (ic/add-input machine input))))

;; Create a "mocked" machine by hard-coding the stream of output from the
;; machine.
(defn- create-mock-machine [m input]
  (assoc m :mock input))

;; Mock-input for the simple example on the problem page.
(def ^:private mock-input (quote ((1 0) (0 0) (1 0) (1 0) (0 1) (1 0) (1 0))))

;; "Run" the robot by repeatedly running/polling the intcode machine until it has
;; halted.
(defn- run-robot [machine robot]
  (loop [robot robot, machine machine]
    (let [m'           (run-machine machine (get-panel robot))
          [paint turn] (:output m')
          m'           (update-state m' :output ())
          r'           (update-robot robot paint turn)]
      (cond
        (:halted m') robot
        :else        (recur r' m')))))

;; For visualizing the paint job, adjust all the points in panels by the offsets
;; given as x and y. This moves all point coordinates to be >= 0.
(defn- adjust-points [panels x y]
  (let [offset (list x y)]
    (reduce (fn [m p]
              (assoc m (map + p offset) (panels p)))
            {} (keys panels))))

;; Display the "raster lines" of the image.
(defn- display [lines]
  (println (str/join "\n" (map #(apply str %) lines))))

;; Display the "code" that was painted on to the panels given in "panels".
(defn- print-code [panels]
  (let [points (keys panels)
        off-x  (abs (apply min (map first points)))
        off-y  (abs (apply min (map last points)))
        panels (adjust-points panels off-x off-y)
        points (keys panels)
        max-x  (apply max (map first points))
        max-y  (apply max (map last points))
        grid   (vec (repeat (inc max-y) (vec (repeat (inc max-x) " "))))
        paint  (map first (get (group-by last panels) 1))]
    (->> (reduce (fn [g p]
                   (assoc-in g (vec (reverse p)) "*"))
                 grid paint)
         (reverse)
         (display))))

;;; Problem 1
(defn p01 [file]
  (as-> file $
    (read-opcodes $)
    (ic/initialize-machine $)
    ;;(create-mock-machine $ mock-input)
    (run-robot $ (create-robot-state))
    (count (keys (:panels $)))))

;;; Problem 2
(defn p02 [file]
  (as-> file $
    (read-opcodes $)
    (ic/initialize-machine $)
    ;;(create-mock-machine $ mock-input)
    (run-robot $ (update-state (create-robot-state) :panels {(list 0 0) 1}))
    (print-code (:panels $))))
