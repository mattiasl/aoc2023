(ns day01
  (:require [clojure.string :refer [join split]]))

(def lines (split (slurp "./src/main/clojure/inputs/day01.in") #"\n"))

(def spelled-digit->digit (zipmap ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"] (iterate inc 1)))

(defn calibration [pattern line]
  (->> (re-seq (re-pattern (str "(?=(" pattern "))")) line)
       ((juxt first last))
       (map last)
       (map #(get spelled-digit->digit % %))
       (join "")
       (Integer/parseInt)))

(reduce + (map (partial calibration "\\d") lines))
(reduce + (map (partial calibration (str "\\d|" (join "|" (keys spelled-digit->digit)))) lines))