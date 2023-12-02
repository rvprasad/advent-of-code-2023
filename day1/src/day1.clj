(ns day1
  (:require [clojure.string :as str]))

(def digits (set "0123456789"))

(defn is-digit [c] 
  (. Character (isDigit c)))         

(defn get-digit [line] 
  (first (filter is-digit (char-array line)))) 

(defn get-num1 [line]
  (let [ldigit (get-digit line)
        rdigit (get-digit (str/reverse line))]
    (parse-long (str/join [ldigit rdigit]))))


(def digits ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "one" "two" 
             "three" "four" "five" "six" "seven" "eight" "nine"])
(def word-to-digits {"one" "1" 
                    "two" "2"
                    "three" "3"
                    "four" "4" 
                    "five" "5" 
                    "six" "6"
                    "seven" "7"
                    "eight" "8"
                    "nine" "9"})

(defn get-indices [index-fn]
  (let [helper (fn [d] [d (index-fn d)])]
     (remove (fn [e] (nil? (second e))) (map helper digits))))

(defn convert-to-sym [e]
  (println e)
  (if (= (count e) 1) e (word-to-digits e)))

(defn get-num2 [line]
  (let [left-indices (get-indices (partial str/index-of line))
        ldigit (convert-to-sym (first (apply min-key second left-indices)))
        right-indices (get-indices (partial str/last-index-of line))
        rdigit (convert-to-sym (first (apply max-key second right-indices)))]
    (parse-long (str/join [ldigit rdigit]))))


(defn -main [filename]
  (with-open [file (clojure.java.io/reader filename)]
    (let [non-empty-lines (remove str/blank? (line-seq file))]
     ;;(println (reduce + (map get-num1 non-empty-lines)))   
     (println (reduce + (map get-num2 non-empty-lines))))))
