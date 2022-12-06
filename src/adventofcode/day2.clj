(ns adventofcode.day2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (slurp (io/file "src/input/day2.txt")))


(defn get-points-for-choice
  [char]
  (cond (= char "X") 1
        (= char "Y") 2
        (= char "Z") 3))

(defn rock-paper-scissors
  "char1: A - rock, B - paper, C - scissors
   char2: X - rock, Y - paper, Z - scissors"
  [char1 char2]
  (if (= char1 "A")
    (if (= char2 "X")
      3
      (if (= char2 "Z") 0 6))
    (if (= char1 "C")
      (if (= char2 "Z")
        3
        (if (= char2 "Y") 0 6))
      (if (= char1 "B")
        (if (= char2 "Y")
          3
          (if (= char2 "X") 0 6))
        -1))))

(defn get-points
  [tuple]
  (vector
   (get-points-for-choice (second tuple))
   (rock-paper-scissors (first tuple) (second tuple))))

(defn modify-answer
  "First: opponent's choice: A - rock, B - paper, C - scissors
   Second: your destiny: X - lose, Y - draw, Z - win"
  [tuple]
  (vector (first tuple)
          (let [char1 (first tuple)
                char2 (second tuple)]
            (if (= char2 "X") ; need to lose
              (if (= char1 "A") "Z"
                  (if (= char1 "B") "X"
                      (if (= char1 "C") "Y")))
              (if (= char2 "Y") ; need draw
                (if (= char1 "A") "X"
                    (if (= char1 "B") "Y"
                        (if (= char1 "C") "Z")))
                (if (= char2 "Z") ; need to win
                  (if (= char1 "A") "Y"
                      (if (= char1 "B") "Z"
                          (if (= char1 "C") "X")))))))))

(def ex1
  (->>
   (str/split input #"\n")
   (map #(str/split % #" "))
   (map #(apply + (get-points %)))
   (apply +)))

ex1

(def ex2
  (->>
   (str/split input #"\n")
   (map #(str/split % #" "))
   (map modify-answer)
   (map #(apply + (get-points %)))
   (apply +)))

ex2