(ns day14
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day14.in") #"\n"))

(defn get-rocks [platform c]
  (->> (map-indexed (fn [y xs]
                      (->> xs
                           (map-indexed (fn [x r]
                                          (when (= r c) [x y])))
                           (remove nil?))) platform)
       (map set)
       (reduce clojure.set/union)))

(defn make-platform [input]
  {:round (get-rocks input \O) :square (get-rocks input \#)})

(defn move-rock [rock squares rounds [mx my] dir]
  (loop [rock rock]
    (let [rock' (mapv + rock dir)
          [x y] rock']
      (if (or (not (< -1 x mx)) (not (< -1 y my)) (contains? squares rock') (contains? rounds rock'))
        rock
        (recur rock')))))

(defn rocks-by-axis [rocks axis]
  (reduce (fn [a c]
            (let [[x y] c
                  val (if (= axis \y) y x)]
              (if (contains? a val)
                (update-in a [val] #(conj % c))
                (assoc a val [c]))))
          (sorted-map)
          rocks))

(defn tilt [squares rounds size dir]
  (let [axis (if (= (first dir) 0) \y \x)
        indexed-by-row (rocks-by-axis rounds axis)
        round-rocks-by-row (if (< 0 (apply + dir)) (reverse indexed-by-row) indexed-by-row)]
    (reduce (fn [a [_ rocks]]
              (apply merge a (reduce (fn [moved rock]
                                       (conj moved (move-rock rock squares moved size dir)))
                                     a
                                     rocks)))
            #{}
            round-rocks-by-row)))

(defn total-load [max-y rocks]
  (->> (map (fn [[_ y]] (- max-y y)) rocks)
       (reduce +)))

(defn size [platform]
  (map inc (reduce (fn [[mx my] [x y]] [(max mx x) (max my y)]) [0 0] (platform :square))))

(defn part1 [input]
  (let [platform (make-platform input)
        [max-x max-y] (size platform)]
    (->> (tilt (platform :square) (platform :round) [max-x max-y] [0 -1])
         ((partial total-load max-y)))))

(defn part2 [input cycles]
  (let [platform (make-platform input)
        [max-x max-y] (size platform)
        dirs [[0 -1] [-1 0] [0 1] [1 0]]]
    (loop [i 0
           round-rocks (platform :round)
           memory #{}
           loads (sorted-map)]
      (let [rocks-after-tilt (reduce (fn [a dir]
                                   (let [tilted (tilt (platform :square) a [max-x max-y] dir)]
                                     tilted))
                                 round-rocks dirs)
            memory' (conj memory rocks-after-tilt)
            index (count memory')
            load (total-load max-y rocks-after-tilt)
            load-by-index (get loads index [])]
        (if (some #(= load %) load-by-index)
          (let [[offset cycle-values] (last loads)
                index (mod (- cycles offset) (count cycle-values))]
            (get cycle-values index))
          (recur (inc i)
                 rocks-after-tilt
                 memory'
                 (assoc loads index (conj load-by-index load))))))))
(comment
  (time (part1 input))
  (time (part2 input 1000000000))
  )