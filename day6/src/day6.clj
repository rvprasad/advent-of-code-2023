(ns day6
  (:require [clojure.string :as str])
  (:require [clojure.core.reducers :as reducers]))

(defn count-winning-solutions [[allowed-time prev-distance]]
  (let [get-wins (fn [l] (filter (partial < prev-distance) l))
        calculate-distance (fn [t] (* t (- allowed-time t)))]
    (count (get-wins (map calculate-distance (range 0 (+ allowed-time 1)))))))

(defn calculation1 [times distances]
  (let [time-distance-pairs
        (into [] (partition 2 (into [] (interleave times distances))))]
    (reducers/fold * (map count-winning-solutions time-distance-pairs))))

(defn calculation2 [times distances]
  (let [time (parse-long (str/join (map str times)))
        distance (parse-long (str/join (map str distances)))]
    (count-winning-solutions [time distance])))

(defn -main [filename]
  (with-open [file (clojure.java.io/reader filename)]
    (let [get-values
          (fn [line]
            (map (comp parse-long str/trim)
                 (str/split (str/trim (second (str/split line #":"))) #" +")))
          non-empty-lines (remove str/blank? (line-seq file))
          times (get-values (first non-empty-lines))
          distances (get-values (second non-empty-lines))]
      (println (calculation1 times distances))
      (println (calculation2 times distances)))))
