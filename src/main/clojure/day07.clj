(ns day07
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day07.in") #"\n"))

(def card->value {1 (zipmap [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] (range 2 15))
                  2 (zipmap [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A] (range 2 15))})

(def type-by-part {1 (fn [freq] (->> (vals freq)
                                     (sort)
                                     (reverse)))
                   2 (fn [freq] (let [jokers (get freq \J 0)]
                                  (if (= jokers 5) [5] (as-> (dissoc freq \J) $
                                                             (vals $)
                                                             (sort $)
                                                             (reverse $)
                                                             (into [] $)
                                                             (update $ 0 + jokers)))))})

(def score
  (memoize
    (fn [part hand]
      (let [freq (frequencies hand)
            type (->> ((get type-by-part part) freq)
                      (take 2)
                      (vec))]
        (cond
          (= type [5]) 6
          (= type [4 1]) 5
          (= type [3 2]) 4
          (= type [3 1]) 3
          (= type [2 2]) 2
          (= type [2 1]) 1
          (= type [1 1]) 0)))))

(def compare-hands
  (memoize (fn [part hand1 hand2]
             (let [[type1 type2] (map (partial score part) [hand1 hand2])]
               (if (not= type1 type2)
                 (- type1 type2)
                 (loop [[h1 & t1] hand1
                        [h2 & t2] hand2]
                   (let [[v1 v2] (map (partial (get card->value part)) [h1 h2])]
                     (if (= v1 v2)
                       (recur t1 t2)
                       (- v1 v2)))))))))

(defn total-winnings
  [input part]
  (->> input
       (map (fn [r] (split r #" ")))
       (map (fn [[hand bid]] {:hand hand :bid (read-string bid)}))
       (sort-by :hand (partial compare-hands part))
       (map-indexed (fn [i {bid :bid}] (* (inc i) bid)))
       (reduce +)))

(time (map #(total-winnings input %) [1 2]))