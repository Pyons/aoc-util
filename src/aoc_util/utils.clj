(ns aoc-util.utils
  "Helper functions"
  (:require [clojure.edn :as edn]
            [clojure.java.io :refer [reader]]
            [hato.client :as hc]))

(defmacro save
  "Executes a body, if an excetption throws, 
  catch and print stacktrace to `*err*` but returns `nil`"
  [& body]
  `(try
     ~@body
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
         ~@body))))

(defn update-vals
  "Updates multiple values in a map by applying the same fn

  ```
  (def user {:year 2020 :age {:n 26}})
  (update-vals user [:year [:age :n]] inc) ;;=> {:year 2021, :age {:n 27}}
  ```"
  [m ks f & args]
  (reduce
    (fn [acc k]
      (let [k (if (vector? k) k [k])]
        (apply update-in acc k f args)))
    m
    ks))

(def
  ^{:doc "Takes a string and tries to parse into an Integer, otherwise nil"
    :arglists '([^String n])}
  str->int
  (memoize
    (fn [^String n]
      (save
        (Integer/parseInt n)))))

(defn numbers-from-str
  "Retrieves all numbers from a string
  returns a list of numbers"
  [^String str]
  (map edn/read-string (re-seq #"\d+" str)))

(defn line-process
  ([^String input] (line-process input identity))
  ([^String input parser]
   (mapv parser (line-seq (reader input)))))
