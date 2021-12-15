(ns aoc2021.day14
  (:require [clojure.string :as str]
            [clojure.set :refer [map-invert]]))

(def input
  (filterv
   not-empty
   (str/split-lines (slurp "/Users/evan.duncan/Work/src/github.com/evan-duncan/aoc2021/src/aoc2021/day14.txt"))))

(defn polymer-template-from [s] (first s))
(defn pair-insertion-rules [s]
  (let [re #"\s+->\s+"
        rules (rest s)]
    (apply hash-map (flatten (map #(str/split %1 re) rules)))))

(defn str->pairs
  "Turn 'NNCB' into ({ ['N' 'N'] 1, ['N' 'C'] 1, ['C' 'B'] 1 })"
  ([str] (str->pairs str 1))
  ([str count]
   (let [parts (str/split str #"")]
     (map (fn [x y] (hash-map [x y] count)) parts (rest parts)))))

(defn merge-seq [seq]
  (apply merge-with + seq))

(defn expand
  "Turn {[N N] 1} => ({[N C] 1} {[C N] 1})"
  [rules result]
  (let [[pair count] result
        [a b] pair
        code (apply str pair)
        element (get rules code)]
    (str->pairs (str a element b) count)))

(defn expand-all [rules results]
  (merge-seq (flatten (map (partial expand rules) results))))

(defn tuple-key->str->keys [m]
  (let [[k v] m]
    (map #(hash-map %1 v) k)))

(defn flatten-expansion [expansion]
  (merge-seq (flatten (map tuple-key->str->keys expansion))))

(defn expansion-range [expansion]
  (let [counts (into (sorted-map) (map-invert (flatten-expansion expansion)))
        totals (keys counts)]
    (quot (- (last totals) (first totals)) 2)))

(defn solve [n]
  (let [template (polymer-template-from input)
        rules (pair-insertion-rules input)
        expand-fn (partial expand-all rules)
        expansion (loop [iter 1
                         frequencies (merge-seq (str->pairs template))]
                    (if (> iter n)
                      frequencies
                      (recur (inc iter) (expand-fn frequencies))))]
    (expansion-range expansion)))

(solve 10)
(solve 40)
