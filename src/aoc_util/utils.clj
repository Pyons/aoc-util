(ns aoc-util.utils
  "Helper functions"
  (:require [clojure.edn :as edn]
            [clojure.java.io :refer [reader]]))

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

(defn line-process
  ([^String input] (line-process input identity))
  ([^String input parser]
   (mapv parser (line-seq (reader input)))))
