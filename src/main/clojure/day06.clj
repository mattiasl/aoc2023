(ns day06
  (:require [clojure.string :refer [join split]]))

(def lines (split (slurp "./src/main/clojure/inputs/day06.in") #"\n"))

(defn parse [races]
  (->> (map #(re-seq #"\d+" %) races)
       (map #(map read-string %))
       (apply map vector)))

(defn count-distances [[t d]]
  (->> (reduce (fn [a c] (let [race-time (- t c)] (conj a (* c race-time)))) [0] (range 1 t))
       (filter #(< d %))
       (count)))

;part 1
(->> (map #(re-seq #"\d+" %) lines)
     (map #(map read-string %))
     (apply map vector)
     (map count-distances)
     (reduce *))

;part 2
(time (->> (map #(re-seq #"\d+" %) lines)
           (map join)
           (mapv read-string)
           (count-distances)))