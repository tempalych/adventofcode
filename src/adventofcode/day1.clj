(ns adventofcode.day1
  (:require [clojure.string :as str]
             [clojure.java.io :as io]))

(def input (slurp (io/file "src/input/day1.txt")))

(def ex1
  (->>
   (str/split input #"\n")
   (map #(when-not (= % "") (Integer. %)))
   (partition-by nil?)
   (remove #(= % '(nil)))
   (map #(reduce + %))
   (apply max)))

ex1

(def ex2
  (->>
   (str/split input #"\n")
   (map #(when-not (= % "") (Integer. %)))
   (partition-by nil?)
   (remove #(= % '(nil)))
   (map #(reduce + %))
   (sort)
   (reverse)
   (take 3)
   (apply +)))

ex2
