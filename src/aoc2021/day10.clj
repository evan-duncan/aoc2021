(ns aoc2021.day10
  (:require [clojure.string :as s]))

(def lines (s/split-lines (slurp "/Users/evan.duncan/Work/src/github.com/evan-duncan/aoc2021/src/aoc2021/day10.txt")))

(def corrupted-chars-points-table
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(def autocomplete-points-table
  {")" 1
   "]" 2
   "}" 3
   ">" 4})

(defn- str->set [str] (into (sorted-set) (s/split str #"")))

(def open (str->set "([{<"))
(def close (str->set ")]}>"))
(def close-char-map (zipmap close open))
(def open-char-map (zipmap open close))

(defn corrupted-chars [chars & {:keys [corrupted stack]
                                :or {corrupted '(), stack '()}}]
  (if (empty? chars)
    [corrupted stack]
    (let [ch (first chars) ch-tail (rest chars)]
      (if (contains? open ch) ;; push open char to stack
        (corrupted-chars ch-tail :corrupted corrupted :stack (cons ch stack))
        (let [stack-char (first stack) stack-tail (rest stack)]
          (if (= stack-char (close-char-map ch))
            (corrupted-chars ch-tail :corrupted corrupted :stack stack-tail)
            (corrupted-chars ch-tail :corrupted (cons ch corrupted) :stack stack-tail)))))))

;; part 1
(->> (map #(first (corrupted-chars (s/split % #""))) lines)
     (map #(corrupted-chars-points-table (first %)))
     (remove nil?)
     (apply +))

;; part 2
(let [coll (->> (map #(corrupted-chars (s/split %1 #"")) lines)
                (map #(when (empty? (first %)) (last %)))
                (remove nil?)
                (map #(replace open-char-map %))
                (map #(replace autocomplete-points-table %))
                (map #(reduce (fn [total n] (+ n (* total 5))) %))
                (sort))]
  (nth coll (quot (count coll) 2)))
