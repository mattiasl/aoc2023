(ns day22
  (:require [clojure.string :refer [split]]
            [clojure.math.combinatorics :as combo]
            [clojure.set :refer [difference union intersection]]))

(def lines (split (slurp "./src/main/clojure/inputs/day22.in") #"\n"))

(defn make-brick [id [x1 y1 z1 x2 y2 z2]]
  (assoc (->> (for [y (range y1 (inc y2))
                    x (range x1 (inc x2))
                    z (range z1 (inc z2))]
                {z #{[x y]}})
              (apply merge-with union))
    :id id))

(defn parse-line [id line]
  (->> (re-seq #"\d+" line)
       (map #(Integer/parseInt %))
       (make-brick id)))

(defn collides? [xys resting-xys]
  (not (empty? (intersection xys resting-xys))))

(defn settle-brick [brick settled-xy-by-z]
  (let [z-min (apply min (filter number? (keys brick)))]
    (loop [z z-min]
      (if (or (= z 0) (collides? (brick z-min) (settled-xy-by-z z)))
        (update-keys brick (fn [k] (if (number? k) (+ 1 z (- k z-min)) k)))
        (recur (dec z))))))

(defn settle-bricks [sorted-bricks]
  (-> (reduce (fn [{:keys [settled settled-xy-by-z]} brick]
                (let [resting-brick (settle-brick brick settled-xy-by-z)]
                  {:settled         (conj settled resting-brick)
                   :settled-xy-by-z (merge-with union (dissoc resting-brick :id) settled-xy-by-z)}))
              {:settled #{} :settled-xy-by-z {}} sorted-bricks)
      :settled))

(defn part-1 [differences]
  (count (filter zero? differences)))

(defn part-2 [differences]
  (reduce + differences))

(defn sort-by-asc-z [bricks]
  (sort-by (fn [brick] (apply min (filter number? (keys brick)))) bricks))

(defn solve [input]
  (let [bricks (sort-by-asc-z (map-indexed parse-line input))
        settled (settle-bricks bricks)
        differences (->> (combo/combinations (sort-by-asc-z settled) (dec (count settled)))
                         (pmap (fn [maybe-not-settled]
                                 (difference (into #{} maybe-not-settled) (settle-bricks maybe-not-settled))))
                         (map count))]
    ((juxt part-1 part-2) differences)))

(comment
  (time (solve lines))
  )