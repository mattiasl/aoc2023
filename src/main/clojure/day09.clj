(ns day09
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day09.in") #"\n"))

(defn extrapolate [sequence]
  (if (every? zero? sequence)
    0
    (let [differences (map (fn [[a b]] (- b a)) (partition 2 1 sequence))]
      (+ (extrapolate differences) (last sequence)))))

(map (fn [f]
       (->> (map #(split % #" ") input)
            (map #(f (map read-string %)))
            (map extrapolate)
            (reduce +)))
     [identity reverse])