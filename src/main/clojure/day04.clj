(ns day04
  (:require [clojure.string :refer [split]]
            [clojure.math]))

(def lines (split (slurp "./src/main/clojure/inputs/day04.in") #"\n"))

(defn parse-cards [cards]
  (reduce (fn [a card]
            (let [parts (split card #"[:|]")
                  id (Integer/parseInt (last (re-seq #"\d+" (first parts))))
                  [wins mine] (map #(set (re-seq #"\d+" %)) (rest parts))]
              (assoc a id (count (clojure.set/intersection wins mine)))))
          {}
          cards))

(defn part1 [cards]
  (->> (parse-cards cards)
       (vals)
       (filter #(> % 0))
       (map #(clojure.math/pow 2 (dec %)))
       (reduce +)))

(defn part2 [cards]
  (->> (reduce (fn [copies [id matching-numbers]]
                 (let [cards (range (inc id) (+ id matching-numbers 1))]
                   (reduce (fn [a id'] (update a id' (fn [x] (+ (or x 1) (get a id 1))))) copies cards)))
               {}
               (into (sorted-map) (parse-cards cards)))
       (vals)
       (reduce +)))

(part1 lines)
(part2 lines)