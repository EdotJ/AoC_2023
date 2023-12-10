(ns day7
  (:require
   [clojure.string :as string]))

(def card-ranks ["2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K", "A"])
(def card-ranks-p2 ["J" "2" "3" "4" "5" "6" "7" "8" "9" "T" "Q" "K", "A"])
(def hand-strengths {:five-of-a-kind 7 :four-of-a-kind 6 :full-house 5 :three-of-a-kind 4 :two-pair 3 :one-pair 2 :high 1})

(defn get-hand-type [hand]
  (let [freqs (reverse (sort-by #(second %) (frequencies (string/split hand #""))))]
    (case (count freqs)
      1 (:five-of-a-kind hand-strengths)
      2 (if (= (second (first freqs)) 4) (:four-of-a-kind hand-strengths) (:full-house hand-strengths))
      3 (if (= (second (second freqs)) 1) (:three-of-a-kind hand-strengths) (:two-pair hand-strengths))
      4 (:one-pair hand-strengths)
      5 (:high hand-strengths))))

(defn get-hands []
  (map #(let [split-string (string/split % #" ")] {:hand (first split-string) :bid (Integer/parseInt (last split-string))}) (string/split (slurp "input.txt") #"\n")))

(defn tie-break-hands [hand1 hand2 card-ranking]
  (let [hand-set1 (:hand hand1)
        hand-set2 (:hand hand2)]
    (if (= hand-set1 hand-set2)
      (<= (:bid hand1) (:bid hand2))
      (loop [idx 0]
        (let [char1 (str (nth hand-set1 idx))
              char2 (str (nth hand-set2 idx))]
          (if (not (= char1 char2))
            (> (.indexOf card-ranking char1) (.indexOf card-ranking char2))
            (recur (inc idx))))))))

(defn compare-hands [hand1 hand2 card-ranking]
  (if (= (:score hand1) (:score hand2)) (tie-break-hands hand1 hand2 card-ranking) (> (:score hand1) (:score hand2))))

(defn score-hand [hand-obj hand-string]
  (assoc hand-obj :score (get-hand-type hand-string)))

(defn replace-joker [hand]
  (let [freqs (filter #(not (= (first %) "J")) (reverse (sort-by #(second %) (frequencies (string/split hand #"")))))]
    (if (empty? freqs)
      "AAAAA"
      (string/replace hand "J" (first (first freqs))))))

(defn solve [opts] (let [input (get-hands)
                         scored-hands (map #(score-hand % (:hand %)) input)
                         sorted-hands (reverse (sort #(compare-hands %1 %2 card-ranks) scored-hands))
                         no-joker-hands (map #(score-hand % (replace-joker (:hand %))) input)
                         sorted-no-joker-hands (reverse (sort #(compare-hands %1 %2 card-ranks-p2) no-joker-hands))]
                     (println (reduce + (map-indexed (fn [idx item] (* (+ idx 1) (:bid item))) sorted-hands)))
                     (println (reduce + (map-indexed (fn [idx item] (* (+ idx 1) (:bid item))) sorted-no-joker-hands)))))