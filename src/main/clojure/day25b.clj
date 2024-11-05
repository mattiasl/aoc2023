(ns day25b
  (:require [clojure.string :refer [split]]
            [clojure.java.shell :refer [sh]]))

(def input (split (slurp "./src/main/clojure/inputs/day25.in") #"\n"))

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

(defn render-graph [input out-file]
  (str "echo \"graph { " (clojure.string/join ";" (reduce (fn [graph row]
                                                            (let [[node neighbours] (split row #": ")]
                                                              (reduce (fn [graph neighbour]
                                                                        (conj graph (str node " -- " neighbour)))
                                                                      graph
                                                                      (split neighbours #" "))))
                                                          []
                                                          input)) "}\" | dot -Tpng -Kneato > " out-file))

(defn part1 [input]
  (let [graph (as-> (make-graph input) %
                    (reduce (fn [graph [a b]]
                              (-> graph
                                  (update a disj b)
                                  (update b disj a)))
                            %
                            [["dhn" "xvh"] ["lxt" "lsv"] ["ptj" "qmr"]]))]
    (* (count (get-all-connected graph "dhn")) (count (get-all-connected graph "xvh")))))

(comment
  ; to install dot
  ; brew install graphviz
  (time (sh "bash" "-c" (render-graph input "graph.png")))
  (time (part1 input))
  )