(ns day01b
  (:require [clojure.string :refer [join split]]))

(def lines (split (slurp "./src/main/clojure/inputs/day01.in") #"\n"))

(defn replace-map [m s] (reduce (fn [a [k v]] (clojure.string/replace a (re-pattern k) v)) s m))

(defn calibration [line] (->> (re-seq #"\d" line)
                              ((juxt first last))
                              (join "")
                              (Integer/parseInt)))

(reduce + (map calibration lines))
(->> (map (partial replace-map {"one"   "o1e"
                                "two"   "t2o"
                                "three" "t3e"
                                "four"  "f4r"
                                "five"  "f5e"
                                "six"   "s6x"
                                "seven" "s7n"
                                "eight" "e8t"
                                "nine"  "n9e"}) lines)
     (map calibration)
     (reduce +))