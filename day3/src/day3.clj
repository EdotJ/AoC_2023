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

(defn find-indices [line number line-num]
  (loop [loop-line line
         found-indices []]
    (let [potential-idx (string/index-of loop-line number)]
      (if (nil? potential-idx)
        found-indices
        (recur (string/replace-first loop-line number (apply str (repeat (count number) "."))) (concat found-indices [{:num number :idx (string/index-of loop-line number) :line-num line-num :end-idx (+ (string/index-of loop-line number) (- (count number) 1))}]))))))

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

(defn find-numbers [line line-num prev next]
  (let [numbers (distinct (re-seq digit-pattern line))]
    (apply concat (map #(find-indices (remove-unnecessary line %) % line-num) numbers))))

(defn find-part-numbers [line line-num prev next]
  (let [indices (find-numbers line line-num prev next)]
    (filter #(is-part-number (- (:idx %) 1) (+ 1 (count (:num %)) (:idx %)) line prev next) indices)))

(defn get-all-part-numbers [lines]
  (apply concat (map-indexed (fn [idx line] (find-part-numbers line idx (get lines (- idx 1)) (get lines (+ idx 1)))) lines)))

(defn part-one [lines]
  (println (reduce + (map #(Integer/parseInt (:num %)) (get-all-part-numbers lines)))))

(defn is-adjacent-num [gear num]
  (let [gear-range [(- (:idx gear) 1) (+ (:idx gear) 1)]
        num-range [(:idx num) (:end-idx num)]]
    (and (<= (first gear-range) (second num-range)) (<= (first num-range) (second gear-range)))))

(defn find-numbers-for-gear [gear all-nums]
  (let [gear-line (:line-num gear)
        nums-by-line (filter #(<= (- gear-line 1) (:line-num %) (+ gear-line 1)) all-nums)]
    (filter #(is-adjacent-num gear %) nums-by-line)))

(defn find-gears [line line-num all-nums]
  (let [potential-gears (find-indices line "*" line-num)]
    (if (empty? potential-gears)
      []
      (let [gear-nums (map #(find-numbers-for-gear % all-nums) potential-gears)]
        (filter #(= (count %) 2) gear-nums)))))

(defn find-gear-ratio [gear-nums]
  (* (Integer/parseInt (:num (first gear-nums))) (Integer/parseInt (:num (last gear-nums)))))

(defn part-two [lines]
  (let [all-nums (apply concat (map-indexed (fn [idx line] (find-numbers line idx (get lines (- idx 1)) (get lines (+ idx 1)))) lines))
        gears  (apply concat (filter #(not (empty? %)) (map-indexed (fn [idx line] (find-gears line idx all-nums)) lines)))]
    (println (reduce + (map find-gear-ratio gears)))))

(defn solve [opts] (let [input (string/split (slurp "input.txt") #"\n")]
                     (part-one input)
                     (part-two input)))