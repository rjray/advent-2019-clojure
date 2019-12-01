(ns advent-2019.day01)

;;; https://adventofcode.com/2019/day/1

;;; Problem 1

;; The simple mass-based fuel calculation, per first half.
(defn- fuel [m]
  (- (int (/ m 3)) 2))

;; Each line in "file" is a number that represents the mass of one module.
;; Total up the fuel needed for each module based on the mass values.
(defn p01 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (map fuel)
       (reduce +)))

;; A semi-recursive mass-based fuel calculation that applies to the fuel needed
;; for the module mass, then the fuel needed for that mass, etc.
(defn- fuel-fuel [mass]
  (let [init (fuel mass)]
    (loop [m init, acc 0]
      (let [f (fuel m)]
        (cond
          (<= f 0) (+ init acc)
          :else    (recur f (+ acc f)))))))

;; Each line in "file" is a number that represents the mass of one module.
;; Total up the fuel needed for each module based on the recursive calculation
;; model.
(defn p02 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (map fuel-fuel)
       (reduce +)))
