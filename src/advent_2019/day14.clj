(ns advent-2019.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/14

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Borrowed from
;; https://github.com/salokristian/advent-of-code-2019/, common.clj.
(defn- find-pred [pred x]
  (some #(when (pred %) %) x))

;; Much of the following adapted from/inspired by
;; https://github.com/salokristian/advent-of-code-2019/, day_14.clj

;; A different take on the parsing than I originally had-- produces a single
;; map for inputs rather than a list of pairs. Output is also (single-key) map.
(defn- parse-recipe [line]
  (->> (str/split line #"=>")
       (map (comp (partial into {})
                  (partial map (fn [[_ amt chem]]
                                 [chem (Integer/parseInt amt)]))
                  (partial re-seq #"(\d+) ([A-Z]+)")))
       (zipmap [:inputs :output])))

;; Turn the lines into a list of recipe specs.
(defn- parse-recipes [lines]
  (map parse-recipe lines))

;; Create a new map with the "count" of all the elements multiplied by x. Used
;; to set the amounts that will be needed to create the requested element for
;; which these are the inputs.
(defn- multiply [elements x]
  (->> elements
       (map (fn [[k v]] [k (* x v)]))
       (into {})))

;; Use the find-pred fn from above to find a specific recipe in the list of
;; all recipes.
(defn- find-recipe [recipes element]
  (let [reactant (fn [recipe]
                   (find-pred (fn [[elt]] (= elt element)) (:output recipe)))
        reaction (find-pred reactant recipes)]
    {:recipe reaction, :produces (second (reactant reaction))}))

;; Get all the requirements for the requested. Tail-recursive, it loops for as
;; long as there are key/value pairs in "requirements". The return value is a
;; map of the results with "ORE" as a key. This is the amount of ore needed to
;; meet all the requirements.
(defn- get-all-requirements [recipes requirements]
  (loop [requirements requirements]
    (if-let [[chemical amount] (find-pred (fn [[chem amt]]
                                            (and (pos-int? amt)
                                                 (not= "ORE" chem)))
                                          requirements)]
      (let [{:keys [recipe produces]} (find-recipe recipes chemical)
            multiple                  (+ (quot amount produces)
                                         (if (zero? (mod amount produces))
                                           0 1))
            requirements'             (merge-with + requirements
                                                  (multiply (:output recipe)
                                                            (* -1 multiple))
                                                  (multiply (:inputs recipe)
                                                            multiple))]
        (recur requirements'))
      requirements)))

;; Calculate the amount of ore needed for n units of fuel.
(defn- ore-for-fuel [recipes n]
  (get-all-requirements recipes {"FUEL" n}))

;; For part 2: Do a binary search over the range of the amount of ore
;; available. The return value is a list of (result partials). If "result" is
;; non-nil, then there was an exact match (i.e., zero ore left over).
;; Otherwise, eventually return with the list of attempted values that didn't
;; exceed the goal number. The caller will sort that list and find the best
;; one.
(defn- search [f goal lo hi]
  (loop [lo lo, hi hi, partials ()]
    (if (< hi lo)
      (list nil partials)
      (let [mid    (quot (+ lo hi) 2)
            result (get (f mid) "ORE")
            latest (conj partials (list mid result))]
        (cond
          (< result goal) (recur (inc mid) hi latest)
          (> result goal) (recur lo (dec mid) partials)
          :else           (list result mid))))))

;;; Problem 1
(defn p01 [file]
  (as-> file $
    (read-lines $)
    (parse-recipes $)
    (ore-for-fuel $ 1)
    (get $ "ORE")))

;;; Problem 2
(defn p02 [file ore]
  (let [recipes (->> file read-lines parse-recipes)
        func    (partial ore-for-fuel recipes)
        [result partials] (search func ore 0 ore)]
    (or result
        (->> partials
             (sort #(compare (first %1) (first %2)))
             (take-while #(< (first %) ore))
             last
             first))))
