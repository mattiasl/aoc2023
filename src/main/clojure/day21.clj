(ns day21
  (:require [clojure.string :refer [split]]
            [day09 :refer [extrapolate]]))

(def lines (split (slurp "./src/main/clojure/inputs/day21.in") #"\n"))

(defn make-garden [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x p] (map-indexed vector row)]
                 {[x y] p})))

(defn all-of-type [garden ch]
  (->> (filter (fn [[_ v]] (= v ch)) garden)
       (keys)
       (set)))

(defn get-neighbours-inf [rocks pos garden-size]
  (reduce (fn [neighbours dir]
            (let [neighbour (mapv + pos dir)
                  centered-neighbour-pos (mapv #(mod % garden-size) neighbour)]
              (if (contains? rocks centered-neighbour-pos)
                neighbours
                (conj neighbours neighbour))))
          #{}
          [[-1 0] [1 0] [0 -1] [0 1]]))

(def part-1 (memoize (fn [steps input]
  (let [garden (make-garden input)
        start (first (all-of-type garden \S))
        garden-size (count (first input))
        rocks (all-of-type garden \#)]
    (->> (reduce (fn [reached _]
                   (reduce (fn [a pos] (apply merge a (get-neighbours-inf rocks pos garden-size))) #{} reached))
                 #{start} (range steps))
         (count))))))

(defn part-2 [steps input]
  (let [garden-size (count (first input))
        non-complete-garden-steps (mod steps garden-size)
        ; calculate the third of the initial sequence of three is what takes most of the time
        sequence (pmap #(time (part-1 (+ non-complete-garden-steps (* % garden-size)) input)) (range 3))
        end (/ (- steps non-complete-garden-steps) garden-size)]
    (->> (range (count sequence) (+ 2 end))
         (reduce (fn [s _] (into [] (rest (conj s (extrapolate s))))) sequence)
         (last))))

(comment
  (time (part-1 64 lines))
  (time (part-2 26501365 lines))
  )