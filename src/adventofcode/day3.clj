(ns adventofcode.day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def input (slurp (io/file "src/input/day3.txt")))

(defn get-priority
  [chr]
  (if (= (str/upper-case chr) chr) ; upper-case
    (+ 27 (str/index-of "abcdefghijklmnopqrstuvwxyz" (clojure.string/lower-case chr)))
    (+ 1  (str/index-of "abcdefghijklmnopqrstuvwxyz" chr))))

(def ex1
  (->>
   (str/split input #"\n")
   (map #(str/split % #""))
   (map #(partition (/ (count %) 2) %))
   (map #(set/intersection (set (first %)) (set (second %))))
   (map first)
   (map get-priority)
   (apply +)))

ex1

(def ex2
  (->>
   (str/split input #"\n")
   (partition 3)
   (map #(set/intersection (set (first %)) (set (second %)) (set (nth % 2))))
   (map first)
   (map str)
   (map get-priority)
   (apply +)))

ex2