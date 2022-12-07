(ns adventofcode.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/file "src/input/day7.txt")))

(defn cd [dir path]
  (cond
    (= dir "/") ["/"]
    (= dir "..") (butlast path)
    :else (concat path [dir])))

(defn make-tree
  [input-lines]
  (loop [current-path ["/"]
         [line & remaining-lines] input-lines
         known-paths #{}
         tree {"/" {}}]
    (if (not (empty? line))
      (case (first line)
        "$" (case (second line)
              "cd" (recur
                    (cd (last line) current-path)
                    remaining-lines
                    (conj known-paths current-path)
                    tree)
              "ls" (recur
                    current-path
                    remaining-lines
                    (conj known-paths current-path)
                    tree))
        "dir" (recur
               current-path
               remaining-lines
               (conj known-paths current-path)
               (update-in
                tree
                current-path
                #(update (or % {}) (last line) (constantly {}))))
        (recur
         current-path
         remaining-lines
         (conj known-paths current-path)
         (update-in
          tree
          current-path
          #(update % (last line) (constantly {:size (Integer. (first line))})))))
      {:tree tree :known-paths known-paths})))

(defn calc-map-size
  [x]
  (if (:size x)
    (:size x)
    (reduce + (map calc-map-size (vals x)))))

(defn calc-sizes
  [data]
  (let [tree (:tree data)
        known-paths (:known-paths data)] 
    (map #(vector % (calc-map-size (get-in tree %))) known-paths))
  )

(def ex1
  (->>
   (str/split input #"\n")
   (map #(str/split % #" "))
   make-tree
   calc-sizes
   (map second)
   (filter #(<= % 100000))
   (apply +)))

ex1


(def ex2
  (let [total 70000000
        update-size 30000000
        sizes (->>
               (str/split input #"\n")
               (map #(str/split % #" "))
               make-tree
               calc-sizes
               (into {}))
        buzy (get sizes ["/"])
        free (- total buzy)
        remaining (- update-size free)]
    (->>
     sizes
     (map second)
     (sort)
     (filter #(>= % remaining))
     (apply min))))

ex2