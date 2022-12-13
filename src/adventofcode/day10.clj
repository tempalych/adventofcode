(ns adventofcode.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/file "src/input/day10.txt")))

(def input-seq (->>
                (str/split input #"\n")
                (map #(str/split % #" "))))

(defn get-pixel
  [cycle sprite-position]
  (let [x (mod cycle 40)] 
;    (println "x:" x)
    (if (< (abs (- x sprite-position)) 2)
      "#"
      ".")
    )
  )

(get-pixel 0 1)

(map #(get-pixel (first %) (second %))
     '([0 0]
       [1 1]
       [2 16]
       [3 16]
       [4 5]
       [5 5]
       [6 11]
       [7 11]
       [8 8]
       [9 8]
       [10 13]
       [11 13]
       [12 12]
       [13 12]
       [14 4]
       [15 4]
       [16 17]
       [17 17]
       [18 21]
       [19 21]))

(defn count-signal
  [input-seq]
  (loop [x 1
         cycle 0
         [command & rest-commands] input-seq
         accum 0
         addx-1st-step-done false
         screen [(get-pixel 0 1)]]
    (if (empty? command)
      {:accum accum :screen screen}
      (let [new-cycle (inc cycle)
            new-accum (if (or
                           (= new-cycle 20)
                           (= (mod (- new-cycle 20) 40) 0))
                        (+ accum (* x new-cycle))
                        accum)
            modifier (if (= (first command) "addx")
                       (Integer. (second command))
                       0)
            addx-1st-step-done (and (false? addx-1st-step-done)
                                    (= (first command) "addx"))
            new-x (if (false? addx-1st-step-done)
                    (+ x modifier)
                    x)
            rest-commands (if (true? addx-1st-step-done)
                            (conj rest-commands command)
                            rest-commands)
            screen (conj screen (get-pixel new-cycle new-x))
            ]
        (recur new-x
               new-cycle
               rest-commands
               new-accum
               addx-1st-step-done
               screen)))))

(def ex1 
  (:accum (count-signal input-seq)))

ex1

(def ex2
  (let [screen (->>
                (count-signal input-seq)
                :screen
                (partition 40 40))]
    (println (nth screen 0))
    (println (nth screen 1))
    (println (nth screen 2))
    (println (nth screen 3))
    (println (nth screen 4))
    (println (nth screen 5)))
  )