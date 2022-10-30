(ns aoc-util.utils-test
  (:require [clojure.test :refer [deftest are is testing]]
            [aoc-util.utils :refer [dijkstra dijkstra-seq shortest-path numbers-from-str str->int]]))

(deftest str-to-int
  (testing "String to Integer"
    (are [x y] (= (str->int x) y)
      nil nil
      5 nil
      "10" 10)))

(deftest line-parsing
  (testing "Line parsing"
    (are [x y] =
      (line-process nil) nil
      (line-process 5) nil
      (line-process "") '()
      (line-process "1\n2\n3\n") ["1" "2" "3"]
      (line-process "1\n2\n3\n" str->int) [1 2 3])))

(deftest UpdateValues
  (testing "Update values inside map"
    (are [x y] =
      (update-vals test-map [:year [:age :n] :d] inc)
      {:year 2021 :age {:n 27} :d 6}
      (update-vals test-map [:year [:age :n] :d] str)
      {:year "2020" :age {:n "26"} :d "5"})))

(deftest numbers-of-string
  (are [x y]  (= (numbers-from-str x) y)
    "" '()
    "test12test21sfsd" '(12 21)
    "1 2" '(1 2)))

(deftest dijkstra-test
  (let [graph {:s {:v 1, :w 4}
               :v {:w 2, :t 6}
               :w {:t 3}}
        res {:s 0, :v 1, :w 3, :t 6}]
    (is (= res
           (dijkstra graph :s)))
    (is (= '([:s 0 [:s]] [:v 1 [:s :v]] [:w 3 [:s :v :w]] [:t 6 [:s :v :w :t]])
           (dijkstra-seq graph :s)))
    (is (= [:s :v :w] (shortest-path graph :s :w)))))
