(ns day21
  (:require [clojure.string :as str]))

(defn get-matching-positions [rows num-rows num-cols match?]
  (let [matching-positions (for [row (range 0 num-rows)
                                 col (range 0 num-cols)
                                 :let [chr (nth (nth rows row) col)]
                                 :when (match? chr)]
                             [[row col]])]
    (set (apply concat matching-positions))))

(defn identify-neighbors [rows]
  (let [num-rows (count rows)
        num-cols (count (first rows))
        plot-positions (get-matching-positions rows num-rows num-cols
                                               (partial str/index-of "S."))
        valid-pos?  (fn [row col]
                      (and (<= 0 row) (< row num-rows)
                           (<= 0 col) (< col num-cols)
                           (contains? plot-positions [row col])))
        get-neighboring-plots (fn [[row col]]
                                [[row col]
                                 (apply vector
                                        (for [[r c] [[-1 0] [1 0] [0 -1] [0 1]]
                                              :when (valid-pos?
                                                     (+ row r) (+ col c))]
                                          [(+ row r) (+ col c)]))])
        neighborhood (->> plot-positions
                          (map get-neighboring-plots)
                          (remove (fn [[_ xs]] (empty? xs)))
                          (apply concat)
                          (apply hash-map))]
    [num-rows num-cols neighborhood]))

(defn find-start-position [rows num-rows num-cols]
  (first (seq (get-matching-positions rows num-rows num-cols (partial = \S)))))

(defn solve-part1 [neighbors num-rows num-cols start-pos num-steps]
  (let [workset (hash-set start-pos)
        take-a-step (fn [positions _]
                      (->> positions
                           (map (partial get neighbors))
                           (apply concat)
                           (set)))
        last-positions (reduce take-a-step (hash-set start-pos) 
                               (range num-steps))]
    (println (count last-positions))))  

(defn -main [filename]
  (with-open [file (clojure.java.io/reader filename)]
    (let [lines (remove str/blank? (line-seq file))
          [num-rows num-cols neighbors] (identify-neighbors lines)
          start-pos (find-start-position lines num-rows num-cols)]
      (solve-part1 neighbors num-rows num-cols start-pos 10))))
