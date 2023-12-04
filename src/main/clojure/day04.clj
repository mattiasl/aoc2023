(ns day04
  (:require [clojure.string :refer [split]]))

(def lines (split (slurp "./src/main/clojure/inputs/day04.in") #"\n"))

(defn pow [a b] (reduce * 1 (repeat b a)))

(defn parse-cards [cards]
  (reduce (fn [a card]
            (let [parts (split card #"[:|]")
                  [wins mine] (map #(set (re-seq #"\d+" %)) (rest parts))
                  id (Integer/parseInt (last (re-seq #"\d+" (first parts))))]
              (assoc a id (count (clojure.set/intersection wins mine)))))
          {}
          cards))

(defn part1 [cards]
  (->> (parse-cards cards)
       (vals)
       (filter #(> % 0))
       (map #(pow 2 (dec %)))
       (reduce +)))

(defn part2 [cards]
  (let [original-cards (into (sorted-map) (parse-cards cards))]
    (->> (reduce (fn [copies [id matching-numbers]]
                   (let [cards (range (inc id) (+ id matching-numbers 1))]
                     (reduce (fn [acc card] (update-in acc [card] (fn [x] (+ x (get acc id))))) copies cards)))
                 (zipmap (range 1 (inc (count original-cards))) (iterate identity 1))
                 original-cards)
         (vals)
         (reduce +))))

(part1 lines)
(part2 lines)