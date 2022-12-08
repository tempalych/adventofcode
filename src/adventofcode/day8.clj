(ns adventofcode.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/file "src/input/day8.txt")))

(defn get-right-row
  [m x y]
  (take-last
   (- (count m) (+ y 1))
   (nth m x))
  )

(defn get-left-row
  [m x y]
  (take y (nth m x))
  )

(defn get-top-col
  [m x y] 
  (take x (map #(nth % y) m))
  )

(defn get-bottom-col
  [m x y]
  (take-last 
   (- (count m) (+ x 1)) 
   (map #(nth % y) m)))

(defn visibility
  [m]
  (loop [x 0
         y 0
         accum 0]
    (let [item (nth (nth m x) y)
          last (- (count m) 1)
          accum (if
                 (or
                  (< (apply max -1 (get-top-col m x y)) item)
                  (< (apply max -1 (get-bottom-col m x y)) item)
                  (< (apply max -1 (get-right-row m x y)) item)
                  (< (apply max -1 (get-left-row m x y)) item))
                  (inc accum)
                  accum)] 
      (if (and
           (= x last)
           (= y last))
        accum
        (if (< y last)
          (recur x (inc y) accum)
          (recur (inc x) 0 accum))))))

(def ex1
  (->>
   (str/split input #"\n")
   (map #(str/split % #""))
   (map (fn [arr] (map #(Integer. %) arr)))
   visibility))

ex1

(defn line-scenic-score
  [tree col]
  (loop [x 0
         accum 0]
    (if (< x (count col))
      (let [item (nth col x)]
        (if (< item tree)
          (recur (inc x) (inc accum))
          (if (>= item tree)
            (recur (count col) (inc accum)))))
      accum)))

(defn scenic-score
  [m]
  (loop [x 0
         y 0
         accum 0]
    (let [item (nth (nth m x) y)
          last (- (count m) 1)
          top-sum (line-scenic-score item (reverse (get-top-col m x y)))
          bottom-sum (line-scenic-score item (get-bottom-col m x y)) 
          right-sum (line-scenic-score item (get-right-row m x y))
          left-sum (line-scenic-score item (reverse (get-left-row m x y)))
          scenic-sum (* top-sum bottom-sum right-sum left-sum)] 
      (if (and
           (= x last)
           (= y last))
        accum
        (if (< y last)
          (recur x (inc y) (max scenic-sum accum))
          (recur (inc x) 0 (max scenic-sum accum)))))))

(def ex2
  (->>
   (str/split input #"\n")
   (map #(str/split % #""))
   (map (fn [arr] (map #(Integer. %) arr)))
   scenic-score))

ex2