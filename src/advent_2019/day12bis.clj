(ns advent-2019.day12bis
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [abs lcm]]))

;;; Some attempts to streamline and/or improve.

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Parse a moon's position vector into a list of three integers.
(defn- parse-position [line]
  (->> line
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))

;; Apply the above over all the lines. The result is a list of lists.
(defn- get-moon-positions [lines]
  (map parse-position lines))

;; My first use of "defstruct"... makes for easier access/manip than a regular
;; map.
(defstruct moon :px :py :pz :vx :vy :vz)

;; Create the list (seq) of moons. Take the positions, tack on [0 0 0] for the
;; velocitys, then create a "moon" struct from the list.
(defn- create-moons [positions]
  (->> positions
       (map #(concat % (list 0 0 0)))
       (map #(apply struct moon %))))

;; Calculate the total energy for one moon.
(defn- calculate-one-energy [moon]
  (let [absm (comp abs moon)]
    (* (reduce + (map absm (list :px :py :pz)))
       (reduce + (map absm (list :vx :vy :vz))))))

;; Total the total energy over all moons.
(defn- calculate-answer [moons]
  (apply + (map calculate-one-energy moons)))

;; Calculate the effect on the velocity from one other moon.
(defn- calc-gravity [m1 m2]
  (-> m1
      (update :vx #(+ % (compare (:px m2) (:px m1))))
      (update :vy #(+ % (compare (:py m2) (:py m1))))
      (update :vz #(+ % (compare (:pz m2) (:pz m1))))))

;; Apply the above over all moons, to update the one moon given.
(defn- apply-calc-gravity [moons moon]
  (reduce calc-gravity moon moons))

;; Update a moon's position based on the current velocity.
(defn- apply-velocity [moon]
  (-> moon
      (update :px #(+ % (:vx moon)))
      (update :py #(+ % (:vy moon)))
      (update :pz #(+ % (:vz moon)))))

;; Update all moons for one time-step.
(defn- update-moons [moons]
  (let [gravity-values (map (partial apply-calc-gravity moons) moons)]
    (doall (map apply-velocity gravity-values))))

;; For part 1: Simulate the moon motion over the specified number of iterations.
;; When done, calculate the total energy over the list of moons.
(defn- simulate-motion [moons iters]
  (loop [moons moons, iter 0]
    (if (= iter iters)
      (calculate-answer moons)
      (recur (update-moons moons) (inc iter)))))

;; For part 2: Find the cycle of one of the axes (x, y, z).
(defn- find-repeat [moons [k1 k2]]
  (loop [moons moons, i 0, seen #{}]
    (let [axis (map #(list (% k1) (% k2)) moons)]
      (cond
        (seen axis) i
        :else       (recur (update-moons moons) (inc i) (conj seen axis))))))

;; Find the repeat-point for the moons. Find the repeat-point for each of the
;; three axes, then calculate the LCM of those three numbers.
(defn- find-repeat-point [moons]
  (reduce lcm (pmap (partial find-repeat moons)
                    [[:px :vx] [:py :vy] [:pz :vz]])))

;;; Problem 1
(defn p01 [file iters]
  (-> file
      read-lines
      get-moon-positions
      create-moons
      (simulate-motion iters)))

;;; Problem 2
(defn p02 [file]
  (-> file
      read-lines
      get-moon-positions
      create-moons
      find-repeat-point))
