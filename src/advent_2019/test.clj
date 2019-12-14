(ns advent-2019.test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- parse-reaction [s]
  (->> (str/split s #"=>")
       (map (comp (partial into {})
                  (partial map (fn [[_ amount chemical]] [chemical (Integer/parseInt amount)]))
                  (partial re-seq #"(\d+) ([A-Z]+)")))
       (zipmap [:in :out])))

(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

(defn parse [file]
  (->> file
       (read-lines)
       (map parse-reaction)))
