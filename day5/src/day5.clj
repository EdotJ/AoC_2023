(ns day5
  (:require
   [clojure.string :as string]))

(def digit-pattern (re-pattern #"\d+"))

(defn parse-range [line]
  (let [split-line (string/split line #" ")] 1
       {:dest-start (Long/parseLong (first split-line)) :src-start (Long/parseLong (nth split-line 1)) :length (Long/parseLong (nth split-line 2))}))

(defn parse-map [map-lines]
  (let [lines (string/split map-lines #"\n")
        map-key (string/replace (first lines) #":" "")
        ranges (map parse-range (drop 1 lines))]
    {:key map-key :ranges ranges}))

(defn convert [number number-map]
  (let [corresponding-range (first (filter #(<= (:src-start %) number (+ (:src-start %) (:length %))) (:ranges number-map)))]
    (if (nil? corresponding-range)
      number
      (+ (- number (:src-start corresponding-range)) (:dest-start corresponding-range)))))

(defn get-location [number maps]
  (loop [loop-num number
         loop-maps maps]
    (if (empty? loop-maps)
      loop-num
      (recur (convert loop-num (first loop-maps)) (drop 1 loop-maps)))))

(defn solve [opts] (let [input (string/split (slurp "input.txt") #"\n\n")
                         seeds (map #(Long/parseLong %) (re-seq digit-pattern (first input)))
                         seeds-p2 (flatten (map #(range (first %) (+ (first %) (second %))) (partition 2 seeds)))
                         maps (map parse-map (drop 1 input))]
                     (println (apply min (map #(get-location % maps) seeds)))
                     (println (apply min (map #(get-location % maps) seeds-p2)))
                     ))