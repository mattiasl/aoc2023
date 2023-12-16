(ns day16
  (:require [clojure.string :refer [split]]))

(def lines (split (slurp "./src/main/clojure/inputs/day16.in") #"\n"))

(defn make-grid [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x cell] (map-indexed vector row)
                     :when (not (= \. cell))]
                 {[x y] cell})))

(def tile+dir->new-dir {\- {[0 1] [[-1 0] [1 0]] [0 -1] [[-1 0] [1 0]]}
                        \| {[1 0] [[0 -1] [0 1]] [-1 0] [[0 -1] [0 1]]}
                        \/ {[1 0] [[0 -1]] [-1 0] [[0 1]] [0 -1] [[1 0]] [0 1] [[-1 0]]}
                        \\ {[1 0] [[0 1]] [-1 0] [[0 -1]] [0 1] [[1 0]] [0 -1] [[-1 0]]}})

(defn expand [beam tile]
  (let [[pos dir] beam
        dirs (if (nil? tile) [dir] (get-in tile+dir->new-dir [tile dir] [dir]))]
    (->> (reduce (fn [beams dir] (conj beams [(mapv + pos dir) dir])) [] dirs)
         (filter (fn [[[x y] _]] (and (<= 0 x 109) (<= 0 y 109)))))))

(defn energized-tiles [grid initial-beams]
  (loop [beams initial-beams
         energized #{}]
    (if (empty? beams)
      (set (map first energized))
      (let [beam (first beams)]
        (if (contains? energized beam)
          (recur (rest beams) energized)
          (let [pos (first beam)]
            (recur (into (rest beams) (expand beam (grid pos))) (conj energized beam))))))))

(defn part2 [input]
  (let [grid (make-grid input)]
    (->> (concat (for [x (range 110)] [[[x 0] [0 1]]])
                 (for [x (range 110)] [[[x 109] [0 -1]]])
                 (for [y (range 110)] [[[0 y] [1 0]]])
                 (for [y (range 110)] [[[109 y] [-1 0]]]))
         (pmap (partial energized-tiles grid))
         (map count)
         (apply max))))

;part1
(time (count (energized-tiles (make-grid lines) [[[0 0] [1 0]]])))

(time (part2 lines))