(ns adventofcode.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/file "src/input/day9.txt")))

(defn sign 
  [x]
  (cond (pos? x) 1
        (neg? x) -1
        :else 0)
  )

(defn move-head
  [current-position direction]
  (let [x (first current-position)
        y (second current-position)]
    (case direction
      "R" [(+ x 1) y]
      "L" [(- x 1) y]
      "U" [x (+ y 1)]
      "D" [x (- y 1)]
      ) 
    ) 
  )

(defn move-tail
  [head tail] 
  (let [head-x (first head)
        head-y (second head)
        tail-x (first tail)
        tail-y (second tail)
        dif-x (- head-x tail-x)
        dif-y (- head-y tail-y)
        ]
    (if (or (> (abs dif-x) 1) (> (abs dif-y) 1))
      (let [new-x (+ tail-x (sign dif-x))
            new-y (+ tail-y (sign dif-y))]
        [new-x new-y])
      tail
      )
    ) 
  )

(defn make-movements
  [directions]
  (loop [head [0 0]
         tail [0 0]
         [movement & rest-movements] directions
         tail-positions #{}]
    (if (empty? movement)
      tail-positions
      (let [new-head (move-head head movement)
            new-tail (move-tail new-head tail)] 
        (recur new-head
               new-tail
               rest-movements
               (conj tail-positions new-tail)))))
  )

(def ex1
  (->>
   (str/split input #"\n")
   (map #(str/split % #" "))
   (map #(repeat (Integer. (second %)) (first %)))
   (apply concat)
   make-movements
   count))

ex1


(defn make-rope-movement
  [rope direction] 
  (loop [[current-knot & rest-knots] rope
         i 0
         current-rope []]
    (if (empty? current-knot)
      current-rope
      (if (= i 0) ; it's head
        (recur rest-knots
               (inc i)
               (conj current-rope (move-head current-knot direction)))
        (recur rest-knots
               (inc i)
               (conj current-rope
                     (move-tail (nth current-rope (- i 1)) current-knot))))))
  )

(defn make-rope-movements
  [rope movements]
  (loop [current-rope rope
         [movement & rest-movements] movements
         tail-positions #{}]
    (if (empty? movement)
      tail-positions
      (let [iteration-rope 
            (make-rope-movement current-rope movement)]
        (recur iteration-rope 
               rest-movements 
               (conj tail-positions (last iteration-rope))))
      )))

(def ex2
  (->>
   (str/split input #"\n")
   (map #(str/split % #" "))
   (map #(repeat (Integer. (second %)) (first %)))
   (apply concat)
   (make-rope-movements (repeat 10 [0 0]))
   count
   )
  )

ex2