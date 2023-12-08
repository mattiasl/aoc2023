(ns day08
  (:require [clojure.string :refer [ends-with? split]]))

(def input (split (slurp "./src/main/clojure/inputs/day08.in") #"\n\n"))

(defn make-graph [graph]
  (reduce (fn [a c]
            (let [[node left right] (re-seq #"[0-9A-Z]+" c)]
              (assoc a node {\L left \R right})))
          {}
          (split graph #"\n")))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm
  ([a b] (cond (zero? a) 0
               (zero? b) 0
               :else (abs (* b (quot a (gcd a b))))))
  ([a b & more] (reduce lcm (lcm a b) more)))

(defn solver [[instructions graph] [start-suffix end-suffix]]
  (let [graph (make-graph graph)
        start-nodes (->> (keys graph)
                         (filter #(ends-with? % start-suffix)))]
    (let [cycles (map (fn [start-node]
                             (reduce (fn [[step node] instruction]
                                       (if (ends-with? node end-suffix)
                                         (reduced step)
                                         [(inc step) (get-in graph [node instruction])]))
                                     [0 start-node]
                                     (cycle instructions)))
                      start-nodes)]
      (if (= 1 (count cycles))
        (first cycles)
        (apply lcm cycles)))))

(time (solver input ["AAA" "ZZZ"]))
(time (solver input ["A" "Z"]))