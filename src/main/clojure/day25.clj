(ns day25
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [union difference]]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.math.combinatorics :as combo]))

(def input (split (slurp "./src/main/clojure/inputs/day25.in") #"\n"))
(def +inf Integer/MAX_VALUE)

(defn make-graph [input]
  (reduce (fn [graph row]
            (let [[node neighbours] (split row #": ")]
              (reduce (fn [graph neighbour]
                        (reduce (fn [graph [a b]]
                                  (assoc graph a (conj (graph a #{}) b)))
                                graph
                                [[node neighbour] [neighbour node]]))
                      graph
                      (split neighbours #" "))))
          {}
          input))

(defn get-all-connected [graph from]
  (loop [queue [from]
         visited #{}]
    (cond
      (empty? queue) visited
      (visited (peek queue)) (recur (pop queue) visited)
      :else (let [node (peek queue)]
              (recur (into (pop queue) (graph node)) (conj visited node))))))

(defn backtrack [from to previous]
  (loop [s '() u to]
    (if (= u from)
      (conj s u)
      (recur (conj s u) (get previous u)))))

(defn dijkstra [from to f visited]
  (loop [q (priority-map from 0)
         distances {}
         previous {}]
    (let [[u dist] (peek q)]
      (cond
        (= u to) (backtrack from to previous)
        (nil? u) []
        :else
        (let [[n p] (reduce (fn [[a p] v]
                                   (let [alt (inc dist)]
                                     (if (< alt (distances v +inf))
                                       [(assoc a v alt) (assoc p v u)]
                                       [a p])))
                                 [{} {}] (difference (into #{} (f u)) visited))]
          (recur (into (pop q) n) (merge-with union distances n) (merge-with union previous p)))))))

(defn find-all-paths [from to graph]
  (loop [visited #{}
         paths #{}]
    (let [path (dijkstra from to graph visited)]
      (if (or (empty? path) (contains? paths path))
        paths
        (recur (union visited (disj (into #{} path) from to)) (conj paths path))))))


(defn remove-edges [graph edges]
  (reduce (fn [graph [edge-a edge-b]]
            (-> graph
                (update edge-a disj edge-b)
                (update edge-b disj edge-a)))
          graph
          edges))

(defn part1 [input]
  (let [graph (make-graph input)
        nodes (keys graph)]
    (->> (loop []
           (let [from (rand-nth nodes)
                 to (rand-nth (into [] (disj (into #{} nodes) from)))
                 paths (find-all-paths from to graph)]
             (if (= (count paths) 3)
               paths
               (recur))))
         (map #(partition 2 1 %))
         (apply combo/cartesian-product)
         (map (fn [edges]
                (->> (first edges)
                     (pmap (partial get-all-connected (remove-edges graph edges)))
                     (map count))))
         (filter (fn [[c1 c2]] (not= c1 c2)))
         (first)
         (apply *))))

(comment
  (time (part1 input))
  )