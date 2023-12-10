(ns day9
  (:require
   [clojure.string :as string]))

(defn make-sequence [nums]
  (loop [curr-nums nums
         sequence []]
    (if (< (count curr-nums) 2)
      sequence
      (recur (drop 1 curr-nums) (concat sequence [(- (second curr-nums) (first curr-nums))])))))

(defn get-prediction [nums]
  (loop [curr-sequence nums
         differences []]
    (if (every? #(= 0 %) curr-sequence)
      (+ (reduce + differences) (last nums))
      (let [new-sequence (make-sequence curr-sequence)]
        (recur new-sequence (concat differences [(last new-sequence)]))))))

(defn get-all-sequences [nums]
  (loop [curr-sequence nums
         sequences []]
    (if (every? #(= 0 %) curr-sequence)
      (concat (reverse sequences) [nums])
      (let [new-sequence (make-sequence curr-sequence)]
        (recur new-sequence (concat sequences [new-sequence]))))))

(defn get-difference [curr-sequence previous-values]
  (let [value-to-subtract (if (empty? previous-values) 
                            (- (second curr-sequence) (first curr-sequence))
                            (last previous-values))]
    (- (first curr-sequence) value-to-subtract)))

(defn extrapolate-backwards [all-sequences]
  (loop [curr-sequences all-sequences
         previous-values []]
    (if (< (count curr-sequences) 2)
      (last previous-values)
      (let []
        (recur (drop 1 curr-sequences) (concat previous-values [(get-difference (second curr-sequences) previous-values)]))))))

(defn solve [opts] (let [histories (map #(map (fn [x] (Integer/parseInt x)) (string/split % #" ")) (string/split (slurp "input.txt") #"\n"))]
                     (println "P1" (reduce + (map get-prediction histories)))
                     (println "P2" (reduce + (map #(extrapolate-backwards (get-all-sequences %)) histories)))
                     ))