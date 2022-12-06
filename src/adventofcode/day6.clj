(ns adventofcode.day6
  (:require [clojure.java.io :as io]))

(def input (slurp (io/file "src/input/day6.txt")))

(defn get-unique-seq-position 
  [message size]
  (+ size
     (->>
      (partition size 1 message)
      (map set)
      (map count)
      (take-while #(not (= size %)))
      count)))

(def ex1 
  (get-unique-seq-position input 4))

ex1

(def ex2
  (get-unique-seq-position input 14))

ex2