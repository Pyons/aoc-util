(ns aoc-util.utils
  "Helper functions"
  (:gen-class
   :name aoc_util.utils
   :prefix "-"
   :main false
   :methods [#^{:static true} [numbersFromStr [String] java.util.List]
             #^{:static true} [strInt [String] Integer]])
  (:import [java.io BufferedReader StringReader])
  (:require [clojure.data.priority-map :refer [priority-map]]
            [com.rpl.specter :as S]
            [clojure.edn :as edn]))

(defmacro save
  "Executes a body, if an excetption throws, 
  catch and print stacktrace to `*err*` but returns `nil`"
  [& body]
  `(try
     (do
       ~@body)
     (catch Exception e#
       (binding [*out* *err*]
         (println (.getMessage e#)))
       (.printStackTrace e#))))

(defmacro define
  "Inspired from scheme to shortcut functions which return a
  lambda function

  ```
  (define f [a] [b] (+ a b))

  ((f 5) 3) ;;=> 8
  ```"
  [name bindings lambda-bindings & body]
  (let [doc-str? (if (string? (first body))
                   (first body)
                   "define scheme inspired")
        body (if (string? (first body))
               (next body)
               body)]
    `(defn ~name ~doc-str?
       [~@bindings]
       (fn [~@lambda-bindings]
         (do
           ~@body)))))

(def
  ^{:doc "Takes a string and tries to parse into an Integer, otherwise nil"
    :arglists '([^String n])}
  str->int
  (memoize
   (fn [^String n]
     (save
      (Integer/parseInt n)))))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn transpose [m]
  (apply mapv vector m))

(defn -strInt [s]
  (str->int s))

(defn numbers-from-str
  "Retrieves all numbers from a string
  returns a list of numbers"
  [^String str]
  (map edn/read-string (re-seq #"\d+" str)))

(defn -numbersFromStr [^String s]
  (numbers-from-str s))

(defn line-process
  "Process a string line by line
  takes a parser fn which is used with mapv, not lazy"
  ([^String input]
   (line-process input (map identity)))
  ([^String input parser-xf]
   (into [] parser-xf (line-seq (BufferedReader. (StringReader. input))))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n), e.g. MAP, should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.
 
  Returns a map from nodes to their distance from start."
  [g start]
  (loop [frontier (priority-map start 0) explored {}]
    (if-let [[v total-cost] (peek frontier)]
      (let [dist (->> (g v)
                      (S/setval [S/MAP-KEYS explored] S/NONE)
                      (S/transform
                       [S/MAP-VALS]
                       (fn [cost] (+ cost total-cost))))]
        (recur (merge-with min (pop frontier) dist) (assoc explored v total-cost)))
      explored)))

(defn dijkstra-seq [g start]
  (letfn [(explore [frontier explored]
            (lazy-seq
             (when-let [[v [total-cost previous-vertex]] (peek frontier)]
               (let [path (conj (explored previous-vertex []) v)
                     dist (->> (g v)
                               (S/setval [S/MAP-KEYS explored] S/NONE)
                               (S/transform
                                [S/MAP-VALS]
                                (fn [cost]
                                  [(+ cost total-cost) v])))
                     frontier (merge-with
                               (fn [a b]
                                 (min-key first a b))
                               (pop frontier)
                               dist)]
                 (cons [v total-cost path]
                       (explore frontier (assoc explored v path)))))))]
    (explore (priority-map start [0]) {})))

(defn shortest-path [g start dest]
  (letfn [(destination? [[vertex]]
            (= vertex dest))]
    (->> (dijkstra-seq g start)
         (drop-while (comp not destination?))
         first
         peek)))

(comment
  (def ug {:s {:v 1, :w 4}
           :v {:w 2, :t 6}
           :w {:t 3}})

  (dijkstra ug :s)

  (dijkstra-seq ug :s)

  (shortest-path ug :s :w))
