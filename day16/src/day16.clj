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

(defn track-beam1 [workset pos-to-mirror visited is-outside?]
  (if (empty? workset)
    visited
    (let [[src] (take 1 workset)
          workset (disj workset src)]
      (if (is-outside? src)
        (track-beam1 workset pos-to-mirror visited is-outside?)
        (let [visited (conj visited src)
              targets (get-targets src pos-to-mirror)
              workset (apply conj workset
                             (filter (fn [x] (not (contains? visited x)))
                                     targets))]
          (println (type workset))
          (track-beam1 workset pos-to-mirror visited is-outside?))))))

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
          ;(println src "->" targets (count visited) workset)
            (recur workset visited)))))))

(defn calculate-energized-tiles [num-rows num-cols pos-to-mirror]
  (let [is-outside? (fn [[row col _]]
                      (or (< row 0) (>= row num-rows)
                          (< col 0) (>= col num-cols)))
        first-col (apply min (map second
                                  (filter (fn [[r _]] (= r 0))
                                          (keys pos-to-mirror))))
        num-tiles-traced-from-first-col
        (count (distinct (map (fn [[r c _]] [r c])
                              (track-beam [0 first-col \R]
                                          pos-to-mirror
                                          is-outside?))))
        tmp1
        (count (distinct (map (fn [[r c _]] [r c])
                              (track-beam1 (hash-set [0 first-col \R])
                                           pos-to-mirror
                                           (hash-set)
                                           is-outside?))))]
    (println (+ first-col num-tiles-traced-from-first-col))))

(defn -main [filename]
  (with-open [file (clojure.java.io/reader filename)]
    (let [lines (remove str/blank? (line-seq file))
          [num-rows num-cols pos-to-mirror] (create-pos-to-mirror lines)]
      ;(println pos-to-mirror)
      (calculate-energized-tiles num-rows num-cols pos-to-mirror))))
