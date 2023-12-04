(ns day4
  (:require [clojure.set :as setops]
            [clojure.string :as string]
            [clojure.math :as math]))

(def digit-pattern (re-pattern #"\d+"))

(defn parse-card [card]
  (let [split-card (string/split card #":")
        card-id (first (re-find digit-pattern (first split-card)))
        numbers (string/split (string/trim (last split-card)) #" \| ")
        winning-numbers (set (re-seq digit-pattern (first numbers)))
        current-numbers (set (re-seq digit-pattern (last numbers)))]
    {:id (Integer/parseInt (str card-id)) :winners winning-numbers :hand current-numbers :intersect (setops/intersection winning-numbers current-numbers)}))


(defn get-scored-card [card]
  (let [intersection (setops/intersection (:winners card) (:hand card))
        score (math/pow 2 (- (count (:intersect card)) 1))]
    (assoc card :score (if (empty? intersection)
                         0
                         score))))

(defn get-copies [card cards]
  (take (count (:intersect card)) (drop (:id card) cards)))

(defn get-cards [card cards]
  (let [copies (get-copies card cards)]
    (concat copies (flatten (map #(get-cards % cards) copies)))))

(defn solve [opts] (let [cards (map parse-card (string/split (slurp "input.txt") #"\n"))
                         scored-cards (map get-scored-card cards)
                         card-ranges (map #(range (+ (:id %) 1) (+ 1 (:id %) (count (:intersect %)))) cards)
                         all-copies (map #(apply concat 
                                                 [%] (drop (- % 1) card-ranges)) 
                                         (flatten card-ranges))]
                    ;;  (println (reduce + (map #(:score %) scored-cards)))
                     (println (flatten all-copies))

                     ))