(ns adventofcode.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/file "src/input/day11.txt")))

(defn parse-items-str
  [items-str]
  (->>
   (str/split (str/trim items-str) #" ")
   rest
   rest
   (map #(str/replace % #"," ""))
   (map #(bigint %))
   vec))

(defn parse-condition
  [condition]
  (let [a (if (= (re-matches #"\d+" (first condition))
                  nil)
             (first condition)
             (bigint (first condition)))
        b (if (= (re-matches #"\d+" (last condition))
                 nil)
            (last condition)
            (bigint (last condition)))
        f (second condition)]
    {:f f
     :a a
     :b b}
    )
  )

(defn parse-operation-str
 [operation-str]
 (->>
  (str/split (str/trim operation-str) #" ")
  rest
  rest
  rest
  parse-condition))

(defn parse-test
  [test-str]
  {:divisible-by (bigint (last (str/split (first test-str) #" ")))
   :true (bigint (last (str/split (second test-str) #" ")))
   :false (bigint (last (str/split (last test-str) #" ")))
   }
)

(defn parse-monkey
  [monkey]
  {:name (first monkey)
   :items (parse-items-str (second monkey))
   :operation (parse-operation-str (nth monkey 2))
   :test (parse-test (take-last 3 monkey))
   :inspected-items 0
   }
  )

(defn get-new-item
  [old operation]
  (let [a (if (= (:a operation) "old")
            old
            (:a operation))
        b (if (= (:b operation) "old")
            old
            (:b operation))
        f (:f operation)]
    (bigint (Math/floor
          (/
           (bigint ((resolve (symbol f)) a b))
           3))))
  )

(defn get-new-item-stressed
  [old operation]
  (let [a (bigint (if (= (:a operation) "old")
                    old
                    (:a operation)))
        b (bigint (if (= (:b operation) "old")
                    old
                    (:b operation)))
        f (:f operation)]
    (mod (bigint ((resolve (symbol f)) a b))
         (* 2 3 5 7 11 13 17 19 23))
    ) ;; https://en.wikipedia.org/wiki/Modular_arithmetic
  )

(def monkeys
  (->>
   (str/split input #"\n\n")
   (map #(str/split % #"\n"))
   (map parse-monkey)
   vec)
  )

(def monkey-state (atom monkeys))
@monkey-state

(defn round
  [destress-fn]
  (loop [iter 0] 
    (when (< iter (count @monkey-state))
      (let [current-monkey (nth @monkey-state iter)
            items (:items current-monkey)
            divisible-by (:divisible-by (:test current-monkey))
            goto-if-true (:true (:test current-monkey))
            goto-if-false (:false (:test current-monkey))] 
        (loop [[current-item & rest-items] items] 
          (when (not (nil? current-item))
            (let [new-item (destress-fn
                            current-item
                            (:operation current-monkey))] 
              (swap! monkey-state
                     update-in [iter :inspected-items]
                     inc) 
              (swap! monkey-state
                     update-in [iter :items]
                     #(vec (rest %))) 
              (if (= 0 (mod new-item divisible-by))
                (swap! monkey-state
                       update-in [goto-if-true :items]
                       conj new-item)
                (swap! monkey-state
                       update-in [goto-if-false :items]
                       conj new-item)) 
              (recur rest-items))))
        (recur (inc iter))))
))

(defn round-1 [] (round get-new-item))

(defn round-2 [] (round get-new-item-stressed))

(repeatedly 20 round-1)
(doall (repeatedly 10000 round-2))

;; ex1
(->>
 @monkey-state
 (map :inspected-items)
 sort
 (take-last 2)
 (apply *)
 )

;; ex2
(->>
 @monkey-state
 (map :inspected-items)
 sort
 (take-last 2)
 (apply *)
 )