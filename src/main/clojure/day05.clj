(ns day05
  (:require [clojure.string :refer [split]]))

(def almanac (split (slurp "./src/main/clojure/inputs/day05.in") #"\n\n"))

(defn map-fn-maker [sort-fn mapping]
  (let [[a b len] (->> (re-seq #"\d+" mapping)
                       (map read-string)
                       (sort-fn))]
    #(when (<= b % (+ b (dec len)))
       (+ % (- a b)))))

(defn make-map-functions [sort-mappings-fn almanac]
  (let [parts (split almanac #"\n")
        functions (mapv (partial map-fn-maker sort-mappings-fn) (rest parts))]
    (fn [x]
      (some identity ((apply juxt (conj functions identity)) x)))))

(defn part1 [almanac]
  (let [seeds (->> (re-seq #"\d+" (first almanac))
                   (map read-string))
        functions (map (partial make-map-functions identity) (rest almanac))]
    (->> (pmap (fn [seed] (reduce (fn [a f] (f a)) seed functions)) seeds)
         (apply min))))

(defn seed-predicate [seeds]
  (let [seed? (->> (re-seq #"\d+" seeds)
                   (map read-string)
                   (partition 2)
                   (map (fn [[x y]] (fn [z] (<= x z (+ x y))))))]
    (fn [x] (some identity ((apply juxt seed?) x)))))

(defn part2 [almanac]
  (let [seed? (seed-predicate (first almanac))
        functions (reverse (map (partial make-map-functions (fn [[dest src len]] [src dest len])) (rest almanac)))]
    (loop [i 1]
      (let [maybe-location (reduce (fn [a f] (f a)) i functions)]
        (if (nil? (seed? maybe-location))
          (recur (* i 2))
          (loop [l (/ i 2) h i]
            (if (<= h (inc l))
              (cond
                (seed? (reduce (fn [a f] (f a)) l functions)) l
                (seed? (reduce (fn [a f] (f a)) h functions)) h)
              (let [m (+ l (quot (- h l) 2))
                    [l' m'] (map #(reduce (fn [a f] (f a)) % functions) [l m])]
                (if (and (nil? (seed? l')) (nil? (seed? m')))
                  (recur m h)
                  (recur l m))))))))))

(time (part1 almanac))
(time (part2 almanac))