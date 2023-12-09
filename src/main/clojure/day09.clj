(ns day09
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day09.in") #"\n"))

(defn get-diff [sequence]
  (->> (partition 2 1 sequence)
       (mapv (fn [[a b]] (- b a)))))

(defn prediction [sequence]
  (loop [history [sequence]]
    (let [last-sequence (peek history)
          freq (frequencies last-sequence)]
      (if (= (get freq 0) (count last-sequence))
        (pop history)
        (recur (conj history (get-diff last-sequence)))))))

(defn extrapolate [part2 history]
  (loop [history history
         value 0]
    (let [last-sequence (peek history)
          history' (pop history)
          value' (if part2
                   (- (first last-sequence) value)
                   (+ value (peek last-sequence)))]
      (if (empty? history')
        value'
        (recur history' value')))))

(map (fn [part]
       (->> (map #(split % #" ") input)
            (mapv (fn [x] (mapv read-string x)))
            (mapv prediction)
            (map (partial extrapolate part))
            (reduce +)))
     [false true])