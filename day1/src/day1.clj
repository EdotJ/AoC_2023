(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn map-numbers [input]
  (loop [numbers {"zero" "0" "one" "1" "two" "2" "three" "3" "four" "4" "five" "5" "six" "6" "seven" "7" "eight" "8" "nine" "9"}
         retVal input]
    (if (empty? numbers)
      retVal
      (let [curr-number (first numbers)
            rest (drop 1 numbers)]
        (recur rest (string/replace retVal
                                    (key curr-number)
                                    (val curr-number)))))))

(defn get-indices [line, numbers, search-func]
  (filter #(some? (val %)) (apply merge (map #(hash-map % (search-func line %)) numbers))))

(defn get-num [line, numbers]
  (let [first-num-indices (get-indices line numbers string/index-of)
        last-num-indices (get-indices line numbers string/last-index-of)]
    (Integer/parseInt
     (map-numbers
      (str
       (key
        (apply min-key val first-num-indices))
       (key
        (apply max-key val last-num-indices)))))))


(defn solve [opts] (let [numbers ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
                         numbers-pt-2 (concat numbers ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"])
                         calibration-values (string/split (slurp "input.txt") #"\n")]
                     (println (reduce + (map #(get-num % numbers) calibration-values)))
                     (println (reduce + (map #(get-num % numbers-pt-2) calibration-values)))))