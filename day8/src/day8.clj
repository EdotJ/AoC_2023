(ns day8
  (:require
   [clojure.string :as string]))

(defn parse-node [node-string]
  (let [split-string (string/split node-string #" = ")
        node-id (first split-string)
        neighbours (string/split (string/replace (second split-string) #"[,\)\(]" "") #" ")]
    [(keyword node-id) {:id node-id :neighbours neighbours}]))

(defn get-next-node [instruction curr-node]
  (if (= instruction "L") (first (:neighbours curr-node)) (second (:neighbours curr-node))))

(defn walk [instructions nodes end-predicate start-node]
  (loop [curr-instructions instructions
         curr-node start-node
         step-count 0]
    (if (end-predicate curr-node) step-count
        (recur (if (= (count (drop 1 curr-instructions)) 0) instructions (drop 1 curr-instructions))
               ((keyword (get-next-node (first curr-instructions) curr-node)) nodes)
               (inc step-count)))))

(defn greatest-common-denominator [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn least-common-multiple
  [a b]
  (/ (* a b) (greatest-common-denominator a b)))

(defn solve [opts] (let [input (string/split (slurp "input.txt") #"\n\n")
                         input-p2 (string/split (slurp "input-2.txt") #"\n\n")
                         instructions (string/split (first input) #"")
                         nodes (apply hash-map (apply concat (map parse-node (string/split (last input) #"\n"))))
                         instructions-p2 (string/split (first input-p2) #"")
                         nodes-p2 (apply hash-map (apply concat (map parse-node (string/split (last input-p2) #"\n"))))
                         a-nodes (map second (filter #(= (last (:id (second %))) \A) nodes-p2))]
                     (println "P1" (walk instructions nodes #(= (:id %) "ZZZ") (:AAA nodes)))
                     (println "P2" (map (fn [node] (walk instructions nodes-p2 #(= (last (:id %)) \Z) node)) a-nodes))))
                     