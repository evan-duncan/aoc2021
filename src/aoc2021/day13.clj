(ns aoc2021.day13
  (:require [clojure.string :as str]
            [clojure.pprint :as prnt]))

(def input
  (str/split-lines (slurp "/Users/evan.duncan/Work/src/github.com/evan-duncan/aoc2021/src/aoc2021/day13.txt")))

(defn points-for [s]
  (set (for [point s :when (some? (re-matches #"\d+,\d+" point))]
         (vec (map #(Integer/parseInt %1) (str/split point #","))))))

(defn folds-for [s]
  (let [re #"[xy]\=\d+"]
    (for [fold s :when (some? (re-find re fold))]
      (let [[axis n] (str/split (re-find re fold) #"=")]
        [axis (Integer/parseInt n)]))))

(defn reflect-point [coord fold-position]
  (if (> coord fold-position)
    (- (* 2 fold-position) coord)
    coord))

(defn x [point position]
  (let [[px py] point]
    [(reflect-point px position) py]))

(defn y [point position]
  (let [[px py] point]
    [px (reflect-point py position)]))

;; part 1
(defn fold-points
  [ps fold]
  (reduce
   (fn [points-set point]
     (let [[axis position] fold]
       (conj points-set (apply (resolve (symbol axis)) `(~point ~position)))))
   #{}
   ps))

(fold-points (points-for input) (first (folds-for input)))

;; part 2
;; (defn recursive-fold-points [points folds]
;;   (for [points points
;;         folds folds]
;;     (if (empty? folds)
;;       points
;;       (let [head (first folds)
;;             tail (rest folds)
;;             pts (fold-points points head)]
;;         (recur pts tail)))))

(defn recursive-fold-points [points folds]
  (if (empty? folds)
    points
    (let [head (first folds)
          tail (rest folds)
          pts (fold-points points head)]
      (recursive-fold-points pts tail))))

(defn matrix-size [s]
  (vector
   (+ 1 (apply max (for [point s :let [[x _] point]] x)))
   (+ 1 (apply max (for [point s :let [[_ y] point]] y)))))

(defn build-matrix [dimensions]
  (let [[x y] dimensions]
    (vec (repeat x (vec (repeat y "."))))))

(let [points (recursive-fold-points (points-for input) (folds-for input))
      matrix (->> points matrix-size build-matrix to-array-2d)]
  (prnt/pprint (reduce (fn [m pt]
                         (let [[x y] pt]
                           (aset m x y "#")
                           m))
                       matrix
                       points)))
