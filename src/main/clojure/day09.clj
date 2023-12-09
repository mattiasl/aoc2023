(ns day09
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day09.in") #"\n"))

(defn extrapolate [part2 sequence]
  (if (every? zero? sequence)
    0
    (let [differences (->> (partition 2 1 sequence)
                           (mapv (fn [[a b]] (- b a))))]
      (if part2
        (- (first sequence) (extrapolate part2 differences))
        (+ (extrapolate part2 differences) (last sequence))))))

(map (fn [part]
       (->> (map #(split % #" ") input)
            (mapv (fn [x] (mapv read-string x)))
            (map (partial extrapolate part))
            (reduce +)))
     [false true])