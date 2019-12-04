(ns advent-2019.day04)

;;; https://adventofcode.com/2019/day/4

;; Are all digits in s in ascending order?
(defn- increasing? [s]
  (let [sq (seq s)]
    (= sq (sort sq))))

;; Is the given value a valid contender?
(defn- valid? [v]
  (let [s (str v)]
    (and (re-find #"(\d)(\1)" s)
         (increasing? s))))

;; Uglier things for part 2:

;; Look for a "proper" pairing: adjacent identical digits that are exactly 2,
;; no longer. Match against the character digit in c.
(defn- proper? [s c]
  (let [pat   (re-pattern (str "(" c ")\\1+"))
        match (re-find pat s)]
    (and match
         (= 2 (count (match 0))))))

;; Look that we have at least one "proper" pair, by mapping the above fn over
;; the nine digits as characters.
(defn- proper-pairs? [s]
  (not (zero? (count (filter identity
                             (map #(proper? s %) (seq "123456789")))))))

;; Check for validity given the additional constraint of part 2.
(defn- very-valid? [v]
  (let [s (str v)]
    (and (proper-pairs? s)
         (increasing? s))))

;;; Problem 1
(defn p01 [lo hi]
  (count (filter valid? (range lo (inc hi)))))

;;; Problem 2
(defn p02 [lo hi]
  (count (filter very-valid? (range lo (inc hi)))))
