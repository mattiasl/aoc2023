(ns day18
  (:require [clojure.string :refer [split]]))

(def lines (split (slurp "./src/main/clojure/inputs/day18.in") #"\n"))

(defn transpose [m]
  (apply mapv vector m))

(defn pick's [area boundary]
  (+ (- area (/ (+ boundary 0) 2)) 1))

(defn shoelace [[vertices boundary]]
  (let [[a b] (transpose (drop-last 1 vertices))
        area (as-> (map (partial reduce +) [(map * (rest b) (drop-last a)) (map * (rest a) (drop-last b))]) $
                   (apply - $)
                   (/ $ 2)
                   (abs $))]
    (+ boundary (pick's area boundary))))

(defn map-line-part1 [[dir n _]]
  (let [str->dir {"R" [1 0] "L" [-1 0] "U" [0 -1] "D" [0 1]}]
    [(str->dir dir) (Integer/parseInt n 10)]))

(defn map-line-part2 [[_ _ hex]]
  (let [str->dir {"0" [1 0] "2" [-1 0] "3" [0 -1] "1" [0 1]}]
    [(str->dir (subs hex 5)) (Integer/parseInt (subs hex 0 5) 16)]))

(defn solver [lines line->dir&n]
  (->> (map #(re-seq #"[a-zA-Z0-9]+" %) lines)
       (reduce (fn [[vertices boundary] line]
                 (let [[dir n] (line->dir&n line)
                       v (mapv + (last vertices) (mapv * [n n] dir))]
                   [(conj vertices v) (+ boundary n)]))
               [[[0 0]] 0])
       (shoelace)))

(time (solver lines map-line-part1))
(time (solver lines map-line-part2))