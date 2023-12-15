(ns day15
  (:require [clojure.string :refer [split]]))

(def steps (split (slurp "./src/main/clojure/inputs/day15.in") #","))

(def snh (memoize (fn [x] (reduce (fn [hash c]
                                    (-> (int c)
                                        (+ hash)
                                        (* 17)
                                        (mod 256)))
                                  0 x))))

(defn eq [label val boxes]
  (let [box (snh label)
        val (read-string val)
        lenses (boxes box {:order []})]
    (assoc boxes box (assoc (if (not (contains? lenses label))
                              (update lenses :order #(conj % label))
                              lenses)
                       label val))))

(defn rm [label boxes]
  (let [box (snh label)
        lenses (update (boxes box) :order (fn [x] (filterv #(not (= label %)) x)))]
    (assoc boxes box (dissoc lenses label))))

(defn focus-power [[box-number box]]
  (reduce (fn [power [slot focus]]
            (+ power (* (inc box-number) slot (box focus))))
          0 (map-indexed (fn [i v] [(inc i) v]) (box :order))))

(defn part2 [steps]
  (->> (reduce (fn [boxes step]
                 (let [[label type val] (rest (first (re-seq #"([a-z]+)(=|-)([0-9]+)?" step)))]
                   (condp = type
                     "=" (eq label val boxes)
                     "-" (rm label boxes))))
               {} steps)
       (map focus-power)
       (reduce +)))

; part1
(->> (map snh steps)
     (reduce +))

(time (part2 steps))