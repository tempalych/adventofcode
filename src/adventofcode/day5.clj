(ns adventofcode.day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (slurp (io/file "src/input/day5.txt")))

; 5.1 
(def start-position
  (->>
   (str/split input #"\n")
   (partition-by #(= % ""))
   (first) 
   (map #(partition 3 4 %))
   (map #(replace {'(\space \space \space) '()} %))
   (mapv (fn [col] (map #(apply str %) col)))
   (map #(replace {"" "[_]"} %))
   (reverse)
   (apply mapv vector)
   (mapv (fn [col] (filter #(not (= % "[_]")) col)))
   (mapv (fn [col] (mapv str/trim col)))))

(defn move
  [position cnt from to]
  (map ;update-in
   (fn [col]
     (if (= (first col) from)
       (drop-last (Integer. cnt) col)
       (if (= (first col) to)
         (concat col (take (Integer. cnt) (reverse (nth position (- (Integer. from) 1)))))
         col)))
   position))

(defn move9001
  [position cnt from to]
  (map ;update-in
   (fn [col]
     (if (= (first col) from)
       (drop-last (Integer. cnt) col)
       (if (= (first col) to)
         (concat col (reverse (take (Integer. cnt) (reverse (nth position (- (Integer. from) 1))))))
         col)))
   position))

(def instructions
  (->>
   (str/split input #"\n") 
   (partition-by #(= % ""))
   (rest) (second)
   (map #(str/split % #" "))
   (map #(vector (nth % 1) (nth % 3) (nth % 5)))))

(defn make-movements [move-fn]
  (loop [rows instructions
         position start-position
         move-fn move-fn]
    (if (empty? rows)
      position
      (recur (drop 1 rows)
             (move-fn position
                      (first (first rows))
                      (second (first rows))
                      (nth (first rows) 2))
             move-fn))))

(def ex1
  (->>
   (make-movements move)
   (map last)
   (map #(subs % 1 2))
   (apply str)))

ex1

(def ex2
  (->>
   (make-movements move9001)
   (map last)
   (map #(subs % 1 2))
   (apply str)))

ex2