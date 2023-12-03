(ns day03
  (:require [clojure.string :refer [split]]))

(def engine-schematics (split (slurp "./src/main/clojure/inputs/day03.in") #"\n"))

(defn adjacent [[xmin xmax] row]
  (let [all (set (for [y (range (dec row) (+ row 2)) x (range (dec xmin) (inc xmax))] [x y]))
        pos (set (for [x (range xmin xmax)] [x row]))]
    (clojure.set/difference all pos)))

(defn parse-engine-schematic [pattern engine-schematic y]
  (let [matcher (re-matcher pattern engine-schematic)]
    (loop [result []]
      (if-not (. matcher find)
        result
        (let [[xmin xmax] [(. matcher start) (. matcher end)]]
          (recur (conj result {:pos      (set (for [x (range xmin xmax)] [x y]))
                               :adjacent (adjacent [xmin xmax] y)
                               :match    (. matcher group)})))))))

(defn adjacent? [this that]
  (not (empty? (clojure.set/intersection (get this :pos) (get that :adjacent)))))

(defn adjacent-numbers [numbers symbol]
  (filter (fn [number] (adjacent? symbol number)) numbers))

(defn adjacent-symbol? [symbols number]
  (not-empty (filter (fn [symbol] (adjacent? number symbol)) symbols)))

(defn gear-ratio [gears]
  (->> (map #(get % :match) gears)
       (map #(Integer/parseInt %))
       (reduce *)))

(defn create-engine [engine-schematics]
  (reduce (fn [state engine-schematic]
            (let [y (get state :y)]
              {:y       (inc y)
               :numbers (into (get state :numbers) (parse-engine-schematic #"\d+" engine-schematic y))
               :symbols (into (get state :symbols) (parse-engine-schematic #"[^\.\d]" engine-schematic y))}))
          {:y 0 :numbers [] :symbols []}
          engine-schematics))

;part 1
(let [engine (create-engine engine-schematics)]
  (->> (get engine :numbers)
       (filter (partial adjacent-symbol? (get engine :symbols)))
       (map #(get % :match))
       (map #(Integer/parseInt %))
       (reduce +)))

;part 2
(let [engine (create-engine engine-schematics)
      gears (->> (get engine :symbols)
                 (filter (fn [s] (= (get s :match) "*")))
                 (map (fn [gears??] (adjacent-numbers (get engine :numbers) gears??)))
                 (filter #(= (count %) 2)))]
  (->> (map gear-ratio gears)
       (reduce +)))