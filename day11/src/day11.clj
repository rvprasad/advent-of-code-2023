(ns day11
  (:require [clojure.string :as str])
  (:require [clojure.core.reducers :as reducers]))

(defn calculate-sum-of-distance [lines scale]
  (let [rows (into [] (map (fn [x] (str/split x #"")) lines))
        expanding-rows (map first (filter (fn [[idx va]] (every? #(= "." %1) va))
                                          (map-indexed list rows)))
        cols (apply mapv vector rows)
        expanding-cols (map first (filter (fn [[idx va]] (every? #(= "." %1) va))
                                          (map-indexed list cols)))
        galaxies (filter (fn [[r c]] (= (get (get rows r) c) "#"))
                         (apply concat (map
                                        (fn [r] (map #(list r %1) (range (count cols))))
                                        (range (count rows)))))
        count-in-range (fn [vals lo hi]
                         (count (filter #(and (< lo %1) (< %1 hi)) vals)))
        calc-distance (fn [[[r1 c1] [r2 c2]]]
                        (let [calc-axis-parallel-distance
                              (fn [a b expaning-elems]
                                (let [num-of-expanding-elems
                                      (count-in-range expaning-elems (min a b) (max a b))
                                      raw-distance (abs (- a b))]
                                  (+ (- raw-distance num-of-expanding-elems)
                                     (* scale num-of-expanding-elems))))]
                          (+ (calc-axis-parallel-distance r1 r2 expanding-rows)
                             (calc-axis-parallel-distance c1 c2 expanding-cols))))
        galaxy-pairs (apply concat (for [g1 galaxies]
                                      (for [g2 galaxies
                                            :when (not (= g1 g2))]
                                        [g1 g2])))]

    (println (/ (reducers/fold + (map calc-distance galaxy-pairs)) 2))))

(defn -main [filename]
  (with-open [file (clojure.java.io/reader filename)]
    (let [lines (remove str/blank? (line-seq file))]
      (calculate-sum-of-distance lines 2)
      (calculate-sum-of-distance lines 1000000))))
