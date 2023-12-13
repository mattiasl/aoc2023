(ns day13
  (:require [clojure.string :refer [join split]]))

(def patterns (split (slurp "./src/main/clojure/inputs/day13.in") #"\n\n"))

(defn transpose [pattern]
  (let [rows (split pattern #"\n")]
    (join "\n" (apply map str rows))))

(defn mirrored-line [pattern mirrored?]
  (let [lines (split pattern #"\n")]
    (loop [maybe-mirror-line 1]
      (if (< maybe-mirror-line (count lines))
        (let [[t b] [(reverse (take maybe-mirror-line lines)) (drop maybe-mirror-line lines)]]
          (if (mirrored? t b)
            maybe-mirror-line
            (recur (inc maybe-mirror-line))))
        0))))

(defn part1? [t b]
  (every? true? (map = t b)))

(defn part2? [t b]
  (let [not-equal-lines (->> (map (fn [top-line bottom-line]
                                    (->> (map = top-line bottom-line)
                                         (filter false?)
                                         (count)
                                         )) t b)
                             (reduce +))]
    (= 1 not-equal-lines)))

(defn solver [patterns pred?]
  (->> (map #((juxt identity transpose) %) patterns)
       (map (fn [[normal transposed]] (+ (* 100 (mirrored-line normal pred?)) (mirrored-line transposed pred?))))
       (reduce +)))

(solver patterns part1?)
(solver patterns part2?)