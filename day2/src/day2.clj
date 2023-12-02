(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def digit-pattern (re-pattern #"(\d+)"))
(def alpha-pattern (re-pattern #"([A-Za-z]+)"))

(defn read-set [set-line]
  (let [cube-setups (string/split set-line #",")]
    (apply merge (map #(hash-map (keyword (first (re-find alpha-pattern %))) (Integer/parseInt (first (re-find digit-pattern %)))) cube-setups))))

(defn read-game [game-line]
  (let [smth (string/split game-line #":")
        sets (map read-set (string/split (last smth) #";"))
        game-id (Integer/parseInt (first (re-find digit-pattern (first smth))))]
    {:id game-id :sets sets}))

(defn is-valid-set [set limits]
  (not (some true? (map #(> (val %) ((key %) limits)) set))))

(defn is-valid-game [game limits]
  (if (not (some false? (map #(is-valid-set % limits) (:sets game)))) (:id game) nil))

(defn find-min-color [color game]
  (apply max (filter #(not (nil? %)) (map color (:sets game)))))

(defn find-min-cubes [game]
  (map #(find-min-color % game) [:red :green :blue]))

(defn find-power [game]
  (reduce * (find-min-cubes game)))

(defn solve [opts] (let [games (string/split (slurp "input.txt") #"\n")
                         parsed-games (map read-game games)
                         cube-limits {:red 12 :green 13 :blue 14}]
                     (println (str "Part 1: " (reduce + (filter #(not (nil? %)) (map #(is-valid-game % cube-limits) parsed-games)))))
                     (println (str "Part 2: " (reduce + (map #(find-power %) parsed-games))))))