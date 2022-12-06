(ns adventofcode.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def input (slurp (io/file "src/input/day4.txt")))

(defn make-ranges
  [ranges]
  (let [x1 (Integer. (first (first ranges)))
        x2 (+ 1 (Integer. (second (first ranges))))
        y1 (Integer. (first (second ranges)))
        y2 (+ 1 (Integer. (second (second ranges))))]
    (vector (set (range x1 x2)) (set (range y1 y2)))))

(defn fully-intersect
  [sets]
  (if (or
       (= #{} (set/difference (first sets) (second sets)))
       (= #{} (set/difference (second sets) (first sets))))
    1
    0))

(defn partly-intersect
  [sets]
  (if (not
       (= #{} (set/intersection (first sets) (second sets))))
    1
    0))


(def ex1
  (->>
   (str/split input #"\n")
   (map #(str/split % #","))
   (map #(vector (str/split (first %) #"-") (str/split (second %) #"-")))
   (map make-ranges)
   (map fully-intersect)
   (apply +)))

ex1

(def ex2
  (->>
   (str/split input #"\n")
   (map #(str/split % #","))
   (map #(vector (str/split (first %) #"-") (str/split (second %) #"-")))
   (map make-ranges)
   (map partly-intersect)
   (apply +)))

ex2