(ns day10
  (:require [clojure.string :refer [split]]))

(def lines (split (slurp "./src/main/clojure/inputs/day10.in") #"\n"))

(defn parse-pipes [pipes]
  (->> (map-indexed (fn [y xs] (->> (map-indexed (fn [x v] {[x y] v}) xs) (remove nil?))) pipes)
       (flatten)
       (apply merge)))

(def pipe->dirs
  {\J {[0 1] [-1 0] [1 0] [0 -1]}
   \F {[0 -1] [1 0] [-1 0] [0 1]}
   \7 {[0 -1] [-1 0] [1 0] [0 1]}
   \L {[0 1] [1 0] [-1 0] [0 -1]}})

(defn next-dir [pipe dir]
  (get-in pipe->dirs [pipe dir] dir))

(defn find-start [pipes]
  (first (first (filter (fn [[_ v]] (= v \S)) pipes))))

(defn get-pipe-loop [pipes start dir]
  (loop [pos start
         dir dir
         visited #{}]
    (if (contains? visited pos)
      visited
      (let [pipe (get pipes pos)
            dir' (next-dir pipe dir)
            pos' (mapv + dir' pos)]
        (recur pos' dir' (conj visited pos))))))

(defn part1 [input]
  (let [pipes (parse-pipes input)
        start (find-start pipes)
        pipe-loop (get-pipe-loop pipes start [0 1])]
    (quot (count pipe-loop) 2)))

(defn part2 [input]
  (let [pipes (parse-pipes input)
        start (find-start pipes)
        pipes (assoc pipes start \|)                        ; TODO: detect this automatically instead
        pipe-loop (get-pipe-loop pipes start [0 1])]
    (reduce (fn [a y]
              (+ a (second (reduce (fn [[e n] x]
                                     (if (contains? pipe-loop [x y])
                                       ; |LJ or |F7
                                       (if (contains? #{\| \J \L} (get pipes [x y]))
                                         [(inc e) n]
                                         [e n])
                                       (if (= 1 (mod e 2))
                                         [e (inc n)]
                                         [e n])))
                                   [0 0]
                                   (range (count (first input)))))))
            0
            (range (count input)))))

(time (part1 lines))
(time (part2 lines))