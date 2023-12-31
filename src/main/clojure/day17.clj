(ns day17
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [union]]
            [clojure.data.priority-map :refer [priority-map]]))

(def lines (split (slurp "./src/main/clojure/inputs/day17.in") #"\n"))

(def +inf (Integer/MAX_VALUE))
(def rotation-matrix {:right [[0 -1] [1 0]] :left [[0 1] [-1 0]] :straight [[1 0] [0 1]]})

(defn make-map [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x loss] (map-indexed vector row)]
                 {[x y] (Character/digit (char loss) 10)})))

(defn grid-size [input]
  [(count (first input)) (count input)])

(defn rot [unit-vec dir-id]
  (reduce (fn [a c] (conj a (reduce + (map * unit-vec c)))) [] (rotation-matrix dir-id)))

(defn get-neighbours [[xm ym] [min-steps max-steps] [pos unit-vec steps]]
  (->> (reduce (fn [neighbours dir-id]
                 (let [unit-vec' (rot unit-vec dir-id)
                       pos' (mapv + pos unit-vec')]
                   (cond
                     (and (= dir-id :straight) (< steps max-steps)) (conj neighbours [pos' unit-vec' (inc steps)])
                     (and (not= dir-id :straight) (<= min-steps steps)) (conj neighbours [pos' unit-vec' 1])
                     :else
                     neighbours)))
               []
               [:left :right :straight])
       (filter (fn [[[x y] _]] (and (< -1 x xm) (< -1 y ym))))))

(defn dijkstra [source target f blocks]
  (let [initial-state (reduce (fn [a [k v]] (assoc a k v)) {} source)]
    (loop [q (into (priority-map) initial-state)
           state initial-state]
      (let [[[u dir steps] loss] (peek q)]
        (if (= u target)
          loss
          (let [result (reduce (fn [a [neighbour dir' steps']]
                                 (let [alt (+ (get state [u dir steps] +inf) (blocks neighbour))
                                       dist (get state [neighbour dir' steps'] +inf)]
                                   (if (< alt dist)
                                     (assoc a [neighbour dir' steps'] alt)
                                     a)))
                               {}
                               (f [u dir steps]))]
            (recur (into (pop q) result) (merge-with union state result))))))))

(defn solve [ranges input]
  (let [blocks (make-map input)
        size (grid-size input)
        source (map (fn [z] [[z z 1] (blocks z)]) [[1 0] [0 1]])
        f (partial get-neighbours size ranges)]
    (dijkstra source (map dec size) f blocks)))

(comment
  (time (solve [1 3] lines))
  (time (solve [4 10] lines))
  )