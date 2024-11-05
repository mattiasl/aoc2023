(ns day23
  (:require [clojure.set :refer [intersection]]
            [clojure.string :refer [split]]))

(def lines (split (slurp "./src/main/clojure/inputs/day23.in") #"\n"))

(defn make-map [trails]
  (apply merge (for [[y row] (map-indexed vector trails)
                     [x trail] (map-indexed vector row)]
                 (when (not= trail \#)
                   {[x y] trail}))))

(defn map-size [input]
  [(count (first input)) (count input)])

(def valid-slope-dirs {\> [1 0] \^ [0 -1] \< [-1 0] \v [0 1]})

(defn trail? [trail]
  (= trail \.))

(defn make-neighbour-adjacency-list-part-1 [trails]
  (reduce (fn [graph pos]
            (let [neighbours (reduce (fn [neighbours dir]
                                       (let [neighbour (mapv + dir pos)
                                             trail (trails neighbour)]
                                         (if (or (trail? trail) (= (valid-slope-dirs trail) dir))
                                           (conj neighbours neighbour)
                                           neighbours)))
                                     #{} [[1 0] [0 -1] [-1 0] [0 1]])]
              (assoc graph pos neighbours)))
          {} (into #{} (keys trails))))

(defn dfs ([source target f] (dfs source target f #{} [source]))
  ([node target f visited local-path]
   (let [visited (conj visited node)]
     (if (= node target)
       [local-path]
       (reduce (fn [paths neighbour]
                 (let [path (dfs neighbour target f visited (conj local-path neighbour))]
                   (into paths path)))
               [] (->> (f node) (filter #(not (contains? visited %)))))))))

(defn dist-to-next-junction [start neighbour graph]
  (loop [distance 1
         neighbour neighbour
         pos start]
    (let [neighbours (graph neighbour)]
      (if (= (count neighbours) 2)
        (recur (inc distance) (first (clojure.set/difference neighbours #{pos})) neighbour)
        [neighbour distance]))))

(defn make-neighbour-adjacency-list-part-2 [trails]
  (reduce (fn [a pos]
            (let [neighbours (->> (reduce (fn [neighbours dir] (conj neighbours (mapv + dir pos)))
                                          #{} [[1 0] [0 -1] [-1 0] [0 1]])
                                  (intersection trails))]
              (assoc a pos neighbours)))
          {} trails))

(defn part-1 [input]
  (let [trails (make-map input)
        size (map-size input)
        neighbour-list (make-neighbour-adjacency-list-part-1 trails)
        target [(- (first size) 2) (- (second size) 1)]]
    (->> (dfs [1 0] target neighbour-list)
         (map count)
         (apply max)
         (dec))))

(defn part-2 [input]
  (let [trails (make-map input)
        size (map-size input)
        target [(- (first size) 2) (- (second size) 1)]
        neighbour-list (make-neighbour-adjacency-list-part-2 (into #{} (keys trails)))
        g (reduce (fn [a [from neighbours]]
                    (reduce (fn [g neighbour]
                              (let [[to dist] (dist-to-next-junction from neighbour neighbour-list)]
                                {:edges      (update (g :edges) #{from to} (fn [best]
                                                                             (if (nil? best)
                                                                               dist
                                                                               (max best dist))))
                                 :neighbours (update (g :neighbours) from (fn [neighbours]
                                                                            (if (nil? neighbours)
                                                                              #{to}
                                                                              (conj neighbours to))))}))
                            a neighbours))
                  {:edges {} :neighbours {}}
                  (filter (fn [[_ v]] (not= 2 (count v))) neighbour-list))]
    (->> (dfs [1 0] target (g :neighbours))
         (pmap (fn [path] (->> (partition 2 1 path)
                               (map (fn [pair] (into #{} pair)))
                               (map (fn [edge] (get (g :edges) edge)))
                               (reduce +))))
         (apply max))))

(comment
  (time (part-1 lines))
  (time (part-2 lines))
  )