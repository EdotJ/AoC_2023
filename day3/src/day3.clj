(ns day3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def digit-pattern (re-pattern #"\d+"))
(def symbol-patttern (re-pattern #"[!@#\$%\^&\*\(\)_\+=\-/]"))

(defn remove-unnecessary [line number]
  (let [bad-nums (filter #(and (not (= number %)) (not (string/includes? number %))) (re-seq digit-pattern line))]
    (loop [ret-val line
           numbers bad-nums]
      (if (empty? numbers)
        ret-val
        (recur (string/replace ret-val (first numbers) (apply str (repeat (count (first numbers)) "."))) (drop 1 numbers))))))

(defn find-indices [line number]
  (loop [loop-line line
         found-indices []]
    (let [potential-idx (string/index-of loop-line number)]
      (if (nil? potential-idx)
        found-indices
        (recur (string/replace-first loop-line number (apply str (repeat (count number) "."))) (concat found-indices [{:num number :idx (string/index-of loop-line number)}]))))))

(defn get-line-within-bounds [start-idx end-idx line]
  (if (> start-idx 0)
    (if (< end-idx (+ 1 (count line)))
      (subs line start-idx end-idx)
      (subs line start-idx))
    (if (< end-idx (+ 1 (count line)))
      (subs line 0 end-idx)
      (subs line 0))))

(defn is-part-number [start-idx end-idx line prev-line next-line]
  (let [search-area (map #(get-line-within-bounds start-idx end-idx %) (filter #(not (nil? %)) [prev-line line next-line]))]
    (some #(not (nil? %)) (map #(re-find symbol-patttern %) search-area))))

(defn find-numbers [line prev next]
  (let [numbers (distinct (re-seq digit-pattern line))
        indices (apply concat (map #(find-indices (remove-unnecessary line %) %) numbers))]
    (filter #(is-part-number (- (:idx %) 1) (+ 1 (count (:num %)) (:idx %)) line prev next) indices)))

(defn part1 [lines]
  (apply concat (map-indexed (fn [idx line] (find-numbers line (get lines (- idx 1)) (get lines (+ idx 1)))) lines)))

(defn find-gear-numbers [line prev next]
  (let [numbers (apply concat (distinct (re-seq digit-pattern line)))
        indices (apply concat (map #(find-indices (remove-unnecessary line %) %) numbers))]
    indices))

(defn is-gear [gear-idx line prev nextt]
  (let [search-area (map #(get-line-within-bounds (- gear-idx 1) (+ gear-idx 1) %) (filter #(not (nil? %)) [prev line next]))]
    (= 2 (count (apply concat (map #(re-seq digit-pattern %) search-area))))))

(defn find-gears-for-line [line prev next]
  (let [gear-indices (filter #(true? (is-gear (:idx %) line prev next)) (find-indices line "*"))]
    (find-gear-numbers line prev next)))

(defn find-gears [lines]
  (apply concat (map #(find-indices % "*") lines)))

(defn solve [opts] (let [input (string/split (slurp "input.txt") #"\n")]
                    ;;  (println (reduce + (map #(Integer/parseInt (:num %)) (part1 input))))
                     (println (find-gears-for-line (get input 1) (get input 0) (get input 2)))
                     ))