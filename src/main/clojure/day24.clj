(ns day24
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :refer [split]]))

(def lines (split (slurp "./src/main/clojure/inputs/day24.in") #"\n"))

(defn line->hailstone [line]
  (->> (re-seq #"[-\d]+" line)
       (mapv bigint)))

(defn extract-two-points-from-trajectory [[px py _ vx vy _]]
  [[px py] [(+ px vx) (+ py vy)]])

(defn time-for-hailstone-x-value [x [px _ _ vx _ _]]
  (/ (- x px) vx))

(defn intersection-point-if-time>0 [[hs1 hs2]]
  (let [[[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]] (map extract-two-points-from-trajectory [hs1 hs2])
        denominator (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))]
    (when (not= denominator 0)
      (let [px (/ (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4)) (* (- x1 x2) (- (* x3 y4) (* y3 x4)))) denominator)
            py (/ (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4)) (* (- y1 y2) (- (* x3 y4) (* y3 x4)))) denominator)
            times (map (partial time-for-hailstone-x-value px) [hs1 hs2])]
        (when (every? #(> % 0) times)
          [px py])))))

(defn sub-matrix [matrix [j i]]
  (reduce (fn [matrix row]
            (conj matrix (vec (keep-indexed #(if (not= %1 i) %2) row))))
          [] (keep-indexed #(if (not= %1 j) %2) matrix)))

(defn det [matrix]
  (if (= (count matrix) 1)
    (let [[[v]] matrix] v)
    (->> (first matrix)
         (map-indexed (fn [column element]
                        (let [sign (if (= (mod column 2) 0) 1 -1)]
                          (* sign element (det (sub-matrix matrix [0 column]))))))
         (reduce +))))

(defn inverse [matrix]
  (let [d (det matrix)]
    (vec (map-indexed (fn [j row]
                        (vec (map-indexed (fn [i _]
                                            (let [sign (if (= (mod (+ i j) 2) 0) 1 -1)]
                                              (* (/ sign d) (det (sub-matrix matrix [i j])))))
                                          row)))
                      matrix))))

(defn dot [v1 v2]
  (->> (mapv * v1 v2)
       (reduce +)))

(defn cross [[i1 j1 k1] [i2 j2 k2]]
  [(- (* j1 k2) (* k1 j2)) (* -1 (- (* i1 k2) (* i2 k1))) (- (* i1 j2) (* i2 j1))])

(defn transpose [m]
  (apply mapv vector m))

(defn matrix-multiply [a b]
  (let [b-transposed (transpose b)]
    (reduce (fn [m row-a]
              (conj m (mapv (fn [col-b]
                              (->> (mapv * row-a col-b)
                                   (reduce +))) b-transposed)))
            [] a)))

(defn make-equation-lines [hsi hsj]
  (let [[pi vi] (split-at 3 hsi)
        [pj vj] (split-at 3 hsj)]
    [[(into (cross (mapv - vi vj) (mapv - pi pj)) [0 0 0])
      (into [0 0 0] (cross (mapv - pi pj) (mapv - vi vj)))]
     [[(dot (mapv - vi vj) (cross pi pj))]
      [(dot (mapv - pi pj) (cross vi vj))]]]))

(defn make-equation-system [[equations answers] [hsi hsj]]
  (let [[equation answer] (make-equation-lines hsi hsj)]
    [(into equations equation)
     (into answers answer)]))

(defn part-1 [lines [min max]]
  (as-> (map line->hailstone lines) $
        (combo/combinations $ 2)
        (map intersection-point-if-time>0 $)
        (filter some? $)
        (filter (fn [w] (every? #(<= min % max) w)) $)
        (count $)))

(defn part-2 [lines]
  (let [[equations answers] (as-> (map line->hailstone lines) $
                                  (shuffle $)
                                  (take 3 $)
                                  (combo/combinations $ 2)
                                  (reduce make-equation-system [] $))]
    (->> (matrix-multiply (inverse equations) answers)
         (take 3)
         (flatten)
         (reduce +))))

(part-1 lines [200000000000000 400000000000000])
(part-2 lines)