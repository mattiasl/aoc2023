(ns day19
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day19.in") #"\n\n"))

(defn parse-rules [rules]
  (->> (split rules #",")
       (map (fn [rule]
              (let [[_ var op val res] (re-find #"([xmas](?=[<>]))?(<|>)?(\d+)?:?(\w+)" rule)]
                {:var var :op op :val (if val (read-string val)) :res res})))))

(defn parse-workflows [workflows]
  (->> (mapv (fn [workflow] (re-find #"(\w+)\{(.*)\}" workflow)) (split workflows #"\n"))
       (reduce (fn [a [_ name rules]]
                 (assoc a name (parse-rules rules))) {})))

(defn parse-parts [parts]
  (->> (split parts #"\n")
       (map (fn [r] (->> (re-seq #"\d+" r)
                         (map read-string))))
       (map (fn [r] (zipmap (map str "xmas") r)))))

(defn condition? [op v1 v2]
  (if (= op "<") (< v1 v2) (> v1 v2)))

(defn process-rules [rules part]
  (loop [rules rules]
    (let [{res :res var :var op :op val :val} (first rules)]
      (if (or (nil? var) (condition? op (part var) val))
        res
        (recur (rest rules))))))

(defn part1 [input]
  (let [workflows (parse-workflows (first input))]
    (reduce (fn [a part]
              (loop [rules (workflows "in")]
                (let [res (process-rules rules part)]
                  (condp = res
                    "A" (apply + a (vals part))
                    "R" a
                    (recur (workflows res))))))
            0 (parse-parts (second input)))))

(defn split-rating [op v [lo hi]]
  (if (= op "<")
    (cond
      (< v lo) {:true [lo hi] :false [hi hi]}
      (< hi v) {:true [lo lo] :false [lo hi]}
      :else
      {:true [lo v] :false [v hi]})
    (cond
      (> lo v) {:true [lo hi] :false [hi hi]}
      (> v hi) {:true [lo lo] :false [lo hi]}
      :else
      {:true [(inc v) hi] :false [lo (inc v)]})))

(defn split-ratings [ratings rules]
  (loop [ratings ratings
         rules rules
         new-ratings []]
    (let [{res :res var :var op :op val :val} (first rules)
          new-rating (assoc ratings :name res)]
      (if (nil? var)
        (conj new-ratings new-rating)
        (let [split (split-rating op val (ratings var))]
          (recur
            (assoc ratings var (split :false))
            (rest rules)
            (conj new-ratings (assoc new-rating var (split :true)))))))))

(defn part2 [input]
  (let [workflows (parse-workflows (first input))]
    (loop [queue [{"x" [1 4001] "m" [1 4001] "a" [1 4001] "s" [1 4001] :name "in"}]
           all-accepted []]
      (if (empty? queue)
        (->> (map (fn [x] (dissoc x :name)) all-accepted)
             (map vals)
             (map (fn [x]
                    (reduce (fn [a [lo hi]]
                              (* a (- hi lo)))
                            1 x)))
             (apply +))
        (let [[head & tail] queue
              rules (workflows (head :name))
              splits (split-ratings head rules)
              [ratings accepted] (reduce (fn [[r a] sr]
                                           (condp = (sr :name)
                                             "A" [r (conj a sr)]
                                             "R" [r a]
                                             [(conj r sr) a]))
                                         [[] []] splits)]
          (recur (concat tail ratings) (concat all-accepted accepted)))))))

(comment
  (part1 input)
  (part2 input)
  )