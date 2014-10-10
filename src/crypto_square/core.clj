(ns crypto-square.core
  (:require [clojure.math.combinatorics :as combo]))

(defn- rotate [mat]
  (apply map list (reverse mat)))

(defn- compile-crypto-square [mat ind]
  (map (partial map (partial = ind)) mat))

(defn- to-crypto-square [mat]
  (let [[tl tr br bl] (map compile-crypto-square
                           (iterate rotate mat)
                           (range))]
    (concat (map concat tl tr)
            (map concat bl br))))

(defn generate
  "Generates all nxn crypto-squares.
   Returns a Lazy Sequence."
  [^Long n]
  {:pre  [(even? n)]}
  (let [len   (/ n 2)
        comb  (- (* len len) 1)]
    (map  (comp to-crypto-square
                (partial partition len)
                (partial cons 0))
          (combo/selections (range 4) comb))))

(defn -main
  "Generates and prints all nxn crypto-squares."
  [^String n]
  (doseq [mat (generate (read-string n))]
    (doseq [row mat]
      (->>  (map #(if % "X" ".") row)
            (apply str)
            println))
    (println)))
