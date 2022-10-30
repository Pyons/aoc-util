(ns aoc-util.tools-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.java.io :as io]
            [aoc-util.utils :refer [str->int]]
            [aoc-util.tools :refer [download-puzzle parse-ns parse-input]]))

(deftest namespace-parser
  (testing "Parsing namespace"
    (are
     [x] (= (parse-ns x) [2020 7])
      "se.ns.ms.2020.day7"
      "se.ns.2020.day7"
      "se.ns.2020.d7"
      "se.2020.d7"
      "se.2020.7"
      "se.2020.0007"
      "se.2020.07"
      "2020.7"
      "2020.d7"
      "2020.day7")))

(deftest test-downloads
  (testing "Downloads Puzzle"
    (let [year 2020
          day 23
          path (format "resources/puzzle/%s/%s.txt" year day)
          f (io/file path)]
      (when (.exists f)
        (io/delete-file f))
      (download-puzzle year day)
      (is (= "789465123\n" (slurp f)))
      (io/delete-file f))))

(deftest parse-input-test
  (testing "Test"
    (is (= ["1" "2"] (parse-input "1\n2\n")))
    (is (= [1 2] (parse-input "1\n2\n" str->int)))))
