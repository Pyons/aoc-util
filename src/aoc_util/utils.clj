(ns aoc-util.utils
  "Helper functions"
  (:require [clojure.edn :as edn]))

(def str->int
  (memoize
   (fn [^String n]
     (try
       (Integer/parseInt n)
       (catch Exception _)))))

(defn numbers-from-str
  "Retrieves all numbers from a string
  returns a list of numbers"
  [^String str]
  (map edn/read-string (re-seq #"\d+" str)))

