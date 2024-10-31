(ns day20
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day20.in") #"\n"))

(defn create-modules [input]
  (reduce (fn [modules line]
            (let [[_ name to] (re-find #"[&%]?(\w+) -> (.*)" line)
                  to (split to #", ")
                  modules (reduce (fn [modules to]
                                    (let [module (modules to {:from #{}})]
                                      (assoc modules to (update module :from conj name))))
                                  modules to)
                  module (merge (modules name) {:to to})]
              (assoc modules name
                             (condp = (first line)
                               \& (merge module {:type :conj :mem {}})
                               \% (merge module {:type :flip :state :off})
                               (merge module {:type :broadcaster})))))
          {} input))

(defn send-pulse [from pulse to]
  (case (to :type)
    :flip (if (= pulse :high)
            [to]
            (if (= (to :state) :off)
              [(assoc to :state :on) :high]
              [(assoc to :state :off) :low]))
    :conj (let [mem (assoc (to :mem) from pulse)
                module (assoc to :mem mem)]
            (if (= (count (module :from)) (count (filter (fn [pulse] (= pulse :high)) (vals mem))))
              [module :low]
              [module :high]))
    [to]))

(defn update-monitored [pulse to pulses monitored button-push-count]
  (if (contains? monitored to)
    (let [pulse-counter (pulses to {:low 0 :high 0})]
      (if (= (pulse-counter :low) 0)
        (assoc pulses to (-> (assoc pulse-counter :button-push-count button-push-count)
                             (update pulse inc)))
        pulses))
    pulses))

(defn push-once [state]
  (let [modules (state :modules)
        button-push-count (state :button-push-count)]
    (loop [queue (->> (get-in modules [(state :start) :to])
                      (mapv (fn [to] [(state :start) :low to])))
           modules modules
           pulses (state :pulses)]
      (if (empty? queue)
        (merge state {:pulses pulses} {:modules modules})
        (let [[[from pulse to] & tail] queue
              pulses (if (state :part1)
                       (update-in pulses [pulse] inc)
                       (update-monitored pulse to pulses (state :monitored) button-push-count))
              [module output] (send-pulse from pulse (modules to))
              tail (if (nil? output)
                     tail
                     (concat tail (mapv (fn [to'] [to output to']) (module :to))))]
          (recur tail (assoc modules to module) pulses))))))

(defn part1 [input]
  (let [times 1000
        state (reduce (fn [state _] (push-once state))
                      {:modules (create-modules input)
                       :start   "broadcaster"
                       :pulses  {:low times :high 0}
                       :part1   true}
                      (range times))]
    (->> (state :pulses)
         (vals)
         (apply *))))

(defn part2 [input]
  (let [modules (create-modules input)
        monitored (as-> (get-in modules ["rx" :from]) %
                        (first %)
                        (get-in modules [% :from]))]
    (->> (reduce (fn [state button-push-count]
                   (let [state (push-once (assoc state :button-push-count button-push-count))
                         monitored-modules (select-keys (state :pulses) monitored)]
                     (if (some zero? (map (fn [x] (x :low) (vals monitored-modules))))
                       state
                       (reduced monitored-modules)))
                   {:modules   (create-modules input)
                    :monitored monitored
                    :start     "broadcaster"
                    :pulses    {}})
                 (range 1 8000))                            ;8000 seems to be enough to find cycle-lengths with my input
         (vals)
         (map (fn [x] (x :button-push-count)))
         (apply *))))

(comment
  (part1 input)
  (part2 input)
  )