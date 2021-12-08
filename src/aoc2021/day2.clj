(ns aoc2021.day2
  (:require [clojure.string :refer [split-lines split]]))

(def planned-course (split-lines (slurp "./day2.text")))

(defn directions [s]
  (let [[k v] (split s #"\s")]
    (hash-map k (Integer/parseInt v))))

(let [{:strs [forward up down]} (apply merge-with + (map directions planned-course))]
  (* forward (- down up)))
