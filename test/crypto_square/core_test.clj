(ns crypto-square.core-test
  (:require [clojure.test :refer :all]
            [crypto-square.core :refer :all]))

(deftest test-generate
  (let [rotate @#'crypto-square.core/rotate]
    (doseq [mat (generate 4)]
      (->>  (iterate rotate mat)
            (take 4)
            (map (partial map (partial map #(if % 1 0))))
            (reduce (partial map (partial map +)))
            (apply concat)
            (every? (partial = 1))
            is))))
