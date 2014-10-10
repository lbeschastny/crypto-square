(ns crypto-square.core
  (:require [clojure.math.combinatorics :as combo]))

(defn- rotate
  "Rotates mat Matrix clockwise"
  [mat]
  (apply map list (reverse mat)))

(defn- compile-sector
  "Converts (k x k) Number matrix
   to (k x k) Boolean matrix"
  [mat ind]
  (map (partial map (partial = ind)) mat))

(defn- to-crypto-square
  "Builds (2k x 2k) Boolean matrix
   from (k x k) Number matrix"
  [mat]
  (let [[tl tr br bl] (map compile-sector
                           (iterate rotate mat)
                           (range))]
    (concat (map concat tl tr)
            (map concat bl br))))

(defn generate
  "Generates all (n x n) crypto-squares.
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
  [^String arg]
  (let [n      (read-string arg)
        border (apply str (repeat (* 3 n) "═"))
        upper  (str "╔═" border "╗")
        lower  (str "╚═" border "╝")]
    (doseq [mat (generate n)]
      (println upper)
      (doseq [row mat]
        (print "║ ")
        (doseq [el row] (print (if el "██ " "   ")))
        (println "║"))
      (println lower))))
