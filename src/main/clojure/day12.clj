(ns day12
  (:require [clojure.string :refer [join split]]))

(def records (split (slurp "./src/main/clojure/inputs/day12.in") #"\n"))

(defn parse-record [n record]
  (let [[springs groups] (split record #" ")]
    [(join "?" (repeat n springs)) (->> (split groups #",")
                                        (map read-string)
                                        (repeat n)
                                        (flatten))]))

(defn valid? [springs [group & _]]
  (and (<= group (count springs))
       (not (some #(= \. %) (take group springs)))
       (or (= group (count springs))
           (not (= (nth springs group) \#)))))

(def count-valid-permutations
  (memoize
    (fn [[springs groups]]
      (if (empty? groups)
        (if (some #(= \# %) springs) 0 1)
        (if-let [[spring & more] springs]
          (+ (if (some #(= spring %) [\. \?])
               (count-valid-permutations [more groups]) 0)
             (if (and (some #(= spring %) [\# \?]) (valid? springs groups))
               (count-valid-permutations [(drop (first groups) more) (rest groups)]) 0))
          0)))))

(time (map (fn [n] (->> (map (partial parse-record n) records)
                        (map count-valid-permutations)
                        (reduce +)))
           [1 5]))