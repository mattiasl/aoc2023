(ns day11
  (:require [clojure.string :refer [join split]]
            [clojure.math.combinatorics]))

(def input (slurp "./src/main/clojure/inputs/day11.in"))

(defn translate [w expandable expansions]
  (->> (filter (fn [z] (< z w)) expandable)
       (count)
       (* expansions)
       (+ w)))

(defn get-galaxies [universe [exs eys] exp]
  (->> (map-indexed (fn [y xs]
                      (->> (map-indexed (fn [x v]
                                          (when (= v \#)
                                            [(translate x exs exp) (translate y eys exp)]))
                                        xs)
                           (remove nil?)))
                    (split universe #"\n"))
       (remove empty?)
       (map set)
       (reduce clojure.set/union)))

(defn transpose [universe]
  (let [rows (split universe #"\n")]
    (join "\n" (apply map str rows))))

(defn get-vertically-expandable [universe]
  (reduce (fn [expandable [i row]]
            (if (re-find #"#" row)
              expandable
              (conj expandable i)))
          #{}
          (map-indexed (fn [i v] [i v]) (split universe #"\n"))))

(defn find-expandable [universe]
  (let [eys (get-vertically-expandable universe)
        exs (->> (transpose universe)
                 (get-vertically-expandable))]
    [exs eys]))

(defn manhattan-distance [[[x1 y1] [x2 y2]]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn solver [universe expansions]
  (let [expandable (find-expandable universe)
        galaxies (get-galaxies universe expandable expansions)]
    (->> (clojure.math.combinatorics/combinations galaxies 2)
         (map manhattan-distance)
         (reduce +))))

(time (solver input 1))
(time (solver input 999999))