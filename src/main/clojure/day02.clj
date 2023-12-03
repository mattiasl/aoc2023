(ns day02
  (:require [clojure.string :refer [split]]))

(def lines (split (slurp "./src/main/clojure/inputs/day02.in") #"\n"))

(defn possible? [[r g b]]
  (and (empty? (filter #(> % 12) r)) (empty? (filter #(> % 13) g)) (empty? (filter #(> % 14) b))))

(defn power [x]
  (reduce (fn [a c] (* a (apply max c))) 1 x))

(defn color-from-line [line color]
  (->> (re-seq (re-pattern (str "(\\d+) " color)) line)
       (map last)
       (map #(Integer/parseInt %))))

(defn line->rgb [line]
  [(color-from-line line "red") (color-from-line line "green") (color-from-line line "blue")])

(defn part1 [lines]
  (->> (map (fn [line]
              (let [id (Integer/parseInt (last (flatten (re-seq #"Game (\d+):" line))))]
                (if (possible? (line->rgb line)) id 0)))
            lines)
       (reduce +)))

(defn part2 [lines]
  (->> (map line->rgb lines)
       (map power)
       (reduce +)))

(part1 lines)
(part2 lines)