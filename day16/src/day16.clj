(ns day16
  (:require [clojure.string :as str]))

(defn create-pos-to-mirror [rows]
  (let [num-rows (count rows)
        num-cols (count (first rows))
        pos-to-mirror (apply hash-map
                             (apply concat
                                    (for [row (range 0 num-rows)
                                          col (range 0 num-cols)
                                          :let [chr (nth (nth rows row) col)]
                                          :when (not (= chr \.))]
                                      [[row col] chr])))]
    [num-rows num-cols pos-to-mirror]))

(defn get-targets [[row col dir] pos-to-mirror]
  (let [sym (pos-to-mirror [row col])]
    (if (nil? sym)
      (condp = dir
        \U [[(- row 1) col dir]]
        \D [[(+ row 1) col dir]]
        \L [[row (- col 1) dir]]
        \R [[row (+ col 1) dir]])
      (condp = [dir sym]
        [\L \-] [[row (- col 1) dir]]
        [\R \-] [[row (+ col 1) dir]]
        [\U \-] [[row (- col 1) \L] [row (+ col 1) \R]]
        [\D \-] [[row (- col 1) \L] [row (+ col 1) \R]]
        [\L \|] [[(- row 1) col \U] [(+ row 1) col \D]]
        [\R \|] [[(- row 1) col \U] [(+ row 1) col \D]]
        [\U \|] [[(- row 1) col dir]]
        [\D \|] [[(+ row 1) col dir]]
        [\L \\] [[(- row 1) col \U]]
        [\R \\] [[(+ row 1) col \D]]
        [\U \\] [[row (- col 1) \L]]
        [\D \\] [[row (+ col 1) \R]]
        [\L \/] [[(+ row 1) col \D]]
        [\R \/] [[(- row 1) col \U]]
        [\U \/] [[row (+ col 1) \R]]
        [\D \/] [[row (- col 1) \L]]))))

(defn track-beam [initial-pos pos-to-mirror is-outside?]
  (loop [workset (hash-set initial-pos) visited (hash-set)]
    (if (empty? workset)
      visited
      (let [[src] (take 1 workset)
            workset (disj workset src)]
        (if (is-outside? src)
          (recur workset visited)
          (let [visited (conj visited src)
                targets (get-targets src pos-to-mirror)
                workset (apply conj workset
                               (filter (fn [x] (not (contains? visited x)))
                                       targets))]
            (recur workset visited)))))))

(defn calculate-energized-tiles [num-rows num-cols
                                 [start-row start-col start-dir]
                                 pos-to-mirror]
  (let [is-outside? (fn [[row col _]]
                      (or (< row 0) (>= row num-rows)
                          (< col 0) (>= col num-cols)))
        positions (keys pos-to-mirror)
        possible-start-positions (if (contains? #{\L \R} start-dir)
                                   (filter (fn [[r _]] (= r start-row))
                                           positions)
                                   (filter (fn [[_ c]] (= c start-col))
                                           positions))
        helper (fn [selector mapper default]
                 (if (empty? possible-start-positions) default
                     (apply selector
                            (map mapper possible-start-positions))))
        initial-position (condp = start-dir
                           \L [start-row (helper max second (- num-cols 1)) \L]
                           \R [start-row (helper min second 0) \R]
                           \U [(helper max first 0) start-col \U]
                           \D [(helper min first (- num-rows 1)) start-col \D])
        initial-span (condp = start-dir
                       \L (- num-cols (second initial-position) 1)
                       \R (second initial-position)
                       \U (- num-rows (first initial-position) 1)
                       \D (first initial-position))
        num-tiles-traced-from-first-col
        (count (distinct (map (fn [[r c _]] [r c])
                              (track-beam initial-position
                                          pos-to-mirror
                                          is-outside?))))]
    (+ initial-span num-tiles-traced-from-first-col)))

(defn calculate-energized-tiles1 [num-rows num-cols pos-to-mirror]
  (calculate-energized-tiles num-rows num-cols [0 0 \R] pos-to-mirror))

(defn calculate-energized-tiles2 [num-rows num-cols pos-to-mirror]
  (let [start-positions 
        (concat (map (fn [x] [x 0 \R]) (range 0 num-rows))
                (map (fn [x] [x (- num-cols 1) \L]) (range 0 num-rows))
                (map (fn [x] [0 x \D]) (range 0 num-cols))
                (map (fn [x] [(- num-rows 1) x \U]) (range 0 num-cols)))
        helper (fn [x] 
                 (calculate-energized-tiles num-rows num-cols x pos-to-mirror))]
    (println (apply max (map helper start-positions)))))

(defn -main [filename]
  (with-open [file (clojure.java.io/reader filename)]
    (let [lines (remove str/blank? (line-seq file))
          [num-rows num-cols pos-to-mirror] (create-pos-to-mirror lines)]
      (calculate-energized-tiles1 num-rows num-cols pos-to-mirror)
      (calculate-energized-tiles2 num-rows num-cols pos-to-mirror))))
