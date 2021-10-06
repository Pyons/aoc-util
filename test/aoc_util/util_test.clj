(ns aoc-util.tool-test
  (:require [clojure.test :refer :all]
            [aoc-util.tools :refer :all]))

(deftest namespace-parser
  (testing "Parsing namespace"
    (are
     [x] (= (parse-ns x) [2020 7])
      "se.ns.ms.2020.day7"
      "se.ns.2020.day7"
      "se.ns.2020.d7"
      "se.2020.d7"
      "se.2020.7"
      "2020.7"
      "2020.d7"
      "2020.day7")))
