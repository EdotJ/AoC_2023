(ns day6
  (:require [clojure.set :as setops]
            [clojure.string :as string]
            [clojure.math :as math]))

(def digit-pattern (re-pattern #"\d+"))

(defn try-race [race-length button-hold-time acceleration]
  (* (* button-hold-time acceleration) (- race-length button-hold-time)))

(defn get-distance-traveled [race acceleration]
  (map #(try-race (:length race) % acceleration) (range 1 (:length race))))

(defn get-beats [race acceleration]
  (count (filter #(< (:distance race) %) (get-distance-traveled race acceleration))))

(defn solve [opts] (let [input (string/split (slurp "input.txt") #"\n")
                         races (map (fn [race-length distance] {:length (Integer/parseInt race-length) :distance (Integer/parseInt distance)})
                                           (re-seq digit-pattern (first input))
                                           (re-seq digit-pattern (last input)))
                         input-p2 {:length (Long/parseLong (apply str(map #(:length %) races))) :distance (Long/parseLong (apply str (map #(:distance %) races)))}
                         acceleration 1]
                     (println (reduce * (map #(get-beats % acceleration) races)))
                     (println input-p2)
                     (println (get-beats input-p2 1))
                     ))