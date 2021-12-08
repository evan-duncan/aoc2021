(ns aoc2021.day2
  (:require [clojure.string :refer [split-lines split]]))

(def planned-course (split-lines (slurp "./day2.text")))

(defn directions [s] (split s #"\s"))

(defn v->m [v]
  (let [[k v] v]
    (hash-map k (Integer/parseInt v))))

;; Part 1
(let [{:strs [forward up down]} (apply merge-with + (map v->m (map directions planned-course)))]
  (* forward (- down up)))


;; Part 2
(def calculations {:aim 0 :position { :x 0 :y 0 }})

(defn down [calcs x]
  (update calcs :aim (fn [n] (+ n x))))

(defn up [calcs x]
  (update calcs :aim (fn [n] (- n x))))

(defn forward [calcs x]
  (merge-with
   (update-in calcs [:position :x] + x)
   (update-in calcs [:position :y] + (* x (get calcs :aim)))))

(reduce
 (fn [calcs d]
   (merge-with (resolve (symbol (first d))) calcs (Integer/parseInt (last d))) calcs)
 calculations
 (map directions planned-course))
