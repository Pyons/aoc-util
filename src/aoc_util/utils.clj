(ns aoc-util.utils
  "Helper functions"
  (:gen-class
   :name aoc_util.utils
   :prefix "-"
   :main false
   :methods [#^{:static true} [numbersFromStr [String] java.util.List]
             #^{:static true} [strInt [String] Integer]])
  (:require
   [clojure.data.priority-map :refer [priority-map priority-map-keyfn]]
   [clojure.edn :as edn]
   [com.rpl.specter :as S])
  (:import
   [java.io BufferedReader StringReader]))

(defmacro save
  "Executes a body, if an excetption throws, 
  catch and print stacktrace to `*err*` but returns `nil`"
  [& body]
  `(try
     (do
       ~@body)
     (catch Exception e#
       (binding [*out* *err*]
         (println (.getMessage e#))
         (.printStackTrace e#)))))

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

(defn parse-int
  "Takes a string or char and tries to parse it into an Integer, otherwise nil"
  [s]
  (cond
    (string? s)
    (try
      (Integer/parseInt s)
      (catch NumberFormatException _ nil))
    (char? s)
    (when (Character/isDigit s)
      (Character/getNumericValue s))
    :else (throw (IllegalArgumentException. "Not a string or char"))))

(defn vec-remove
  "remove elem in coll, returns a subvector"
  [coll pos]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn transpose [m]
  (apply mapv vector m))

(defn split-by
  "Applies f to each value in coll, splitting it each time f returns 
  true, removing the value from the result. Returns a lazy seq of
  partitions. Returns a stateful transducer when no collection is
  provided."
  {:static true}
  ([pred]
   (fn [rf]
     (let [a (java.util.ArrayList.)]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.isEmpty a)
                         result
                         (let [v (vec (.toArray a))]
                           (.clear a)
                           (unreduced (rf result v))))]
            (rf result)))
         ([result input]
          (if (pred input)
            (if (.isEmpty a)
              result
              (let [p (vec (.toArray a))]
                (.clear a)
                (rf result p)))
            (do
              (.add a input)
              result)))))))
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [t (take-while (complement pred) s)]
        (if (empty? t)
          (split-by pred (drop-while pred s))
          (cons t (split-by pred (lazy-seq (drop (count t) s))))))))))

(defn indexed-coll
  "Takes a coll and returns a lazy-seq of vectors [i v]
  where v is a value from the input coll "
  [coll]
  (if (counted? coll)
    (map vector (range (long (count coll))) coll)
    (map vector (range) coll)))

(defn index-matrix [matrix]
  (map-indexed
   (fn [x row]
     (map-indexed
      (fn [y value]
        [[x y] value])
      row))
   matrix))

(defn sum [xs]
  (reduce + 0 xs))

(defn nth-xy
  ([coll [x y]]
   (nth-xy coll x y))
  ([coll x y]
   (nth (nth coll y nil) x nil)))

(defn get-column [matrix n]
  (nth (transpose matrix) n))

(defn aget-column [amatrix-2d n]
  (areduce amatrix-2d idx ret []
           (conj ret (aget amatrix-2d n idx))))

(defn get-column-fn [matrix]
  (let [matrix (transpose matrix)]
    (fn [n]
      (nth (transpose matrix) n))))

(defn get-row [matrix n]
  (nth matrix n))

(defn -strInt [s]
  (parse-int s))

(defn numbers-from-str
  "Retrieves all numbers from a string
  returns a list of numbers"
  [^String str]
  (map edn/read-string (re-seq #"\d+" str)))

(defn -numbersFromStr [^String s]
  (numbers-from-str s))

(defn line-process
  "Process a string line by line
  takes a parser fn (transducer) which is used with mapv, not lazy"
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
    (let [xf (comp
              (drop-while (comp not destination?))
              (take 1))]
      (peek
       (first
        (transduce
         xf
         conj [] (dijkstra-seq g start)))))))

(defn euclid-distance [[ye xe]]
  (memoize
   (fn [n]
     (let [[yp xp] (key n)]
       (Math/sqrt (+ (Math/pow (- xe xp) 2)
                     (Math/pow (- ye yp) 2)))))))

(defn- reconstruct-path [came-from current]
  (loop [current current path (list (key current))]
    (if-let [next-current (came-from (key current))]
      (recur next-current (conj path (key next-current)))
      path)))

(defn- make-mapentry [k v]
  (first {k v}))

(defn a*
  "A* algorithm, accepts the same arguemnts as the dijkstra implementation, but addtionally 
  a function `h` which runs a heuristic. Not lazy"
  ([graph start goal]
   (a* graph start goal (constantly 0)))
  ([graph start goal h]
   (loop [open-set (priority-map-keyfn first start [(h (make-mapentry start 0)) 0])
          current (peek open-set)
          came-from (transient {})
          closed #{start}
          steps 0]
     (cond
       (= (key current) goal)
       (do
         (println "Steps:" steps)
         (reconstruct-path (persistent! came-from) current))
       (seq open-set)
       (let [[new-open-set new-came-from]
             (reduce
              (fn [[open-set came-from] [neighbor-key neighbor-val :as neighbor]]
                (let [tentative-g-score (+ (second (val current))  neighbor-val)]
                  (if (< tentative-g-score (or (second (open-set neighbor-key)) ##Inf))
                    [(assoc open-set neighbor-key [(+ tentative-g-score (h neighbor)) tentative-g-score])
                     (assoc! came-from neighbor-key current)]
                    [open-set came-from])))
              [(pop open-set) came-from]
              (into []
                    (remove (fn [n]
                              (closed (key n))))
                    (graph (key current))))
             new-current (peek new-open-set)]
         (recur new-open-set new-current new-came-from (conj closed (key new-current)) (inc steps)))))))

(comment
  (def ug {:s {:v 1, :w 4}
           :v {:w 2, :t 6}
           :w {:t 3}})

  (require '[clojure.string :as str])

  (def test-input (line-process "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"))
  (defn create-graph [m]
    (let [idx (for [y (range (count m))
                    x (range (count (first m)))]
                [y x])
          g (reduce
             (fn [*g [y x]]
               (let [curr (get-in m [y x])
                     edges [[(dec y) x] [(inc y) x] [y (dec x)]  [y (inc x)]]]
                 (assoc! *g [y x] (reduce
                                   (fn [es [y' x']]
                                     (if-let [e (get-in m [y' x'])]
                                       (if (or (<= curr e (inc curr))
                                               (< e curr))
                                         (assoc es [y' x'] 1)
                                         es)
                                       es))
                                   {} edges))))
             (transient {})
             idx)]
      (persistent! g)))

  (defn parse-input' [xs]
    (let [starting-pos (volatile! nil)
          final-position (volatile! nil)
          height (count xs)
          width (count (first xs))
          result-map (to-array-2d (repeat height (repeat width "")))]
      (loop [xs xs row 0]
        (if-not (seq xs)
          {:starting-pos @starting-pos :final-position @final-position :m result-map}
          (do
            (doseq [[idx [c]] (map-indexed vector (str/split (first xs) #""))]
              (cond
                (= c \S) (do
                           (vreset! starting-pos [row idx])
                           (aset result-map row idx 0))
                (= c \E) (do  (vreset! final-position [row idx])
                              (aset result-map row idx 26))
                :else (aset result-map row idx (- (int c) 96))))
            (recur (next xs) (inc row)))))))

  (time (count (shortest-path (create-graph (:m (parse-input' test-input))) [0 0] [2 5])))

  (apply reconstruct-path (a* ug :s :t (fn [n] 10)))

  (let [input (parse-input' test-input)
        g (create-graph (:m input))
        start (:starting-pos input)
        end (:final-position input)]
    (a* g start end  (euclid-distance end)))

  (defn h [m end]
    (let [f (euclid-distance end)]
      (memoize (fn [n]
                 (let [p (key n)
                       dist (f n)
                       height (get-in m p)]
                   (- dist height))))))

  (defn h' [m]
    (memoize
     (fn [n]
       (- 30 (get-in m (key n))))))

  (let [input (parse-input' (line-process (slurp "/home/steffen/Documents/priv/clojure/advent-of-code/resources/puzzle/2022/12.txt")))
        g (create-graph (:m input))
        start (:starting-pos input)
        end (:final-position input)]
    (println "dijkstra")
    (time
     (count (shortest-path g start end)))
    (println "a* euclid-distance")
    (time
     (count (a* g start end (euclid-distance end))))
    (println "a* constantly 0")
    (time
     (count (a* g start end)))
    (println "consider distance and height")
    (time
     (count (a* g start end (h (:m input) end))))
    (println "only consider height")
    (time
     (count (a* g start end (h' (:m input))))))

  (let [input (parse-input' (line-process (slurp "/home/steffen/Documents/priv/clojure/advent-of-code/resources/puzzle/2022/12.txt")))
        g (create-graph (:m input))
        start (:starting-pos input)
        end (:final-position input)]
    (time
     (count (shortest-path g start end))))

  (priority-map :s 0)
  (dijkstra ug :s)

  (dijkstra-seq ug :s)

  (shortest-path ug :s :t))
