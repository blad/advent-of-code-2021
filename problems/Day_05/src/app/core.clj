(ns app.core
  (:require [clojure.string :as str])
  (:require [app.input :refer :all]) 
  (:require [app.utils :refer :all]) 
  (:gen-class))

(defn tally [acc [x y]]
  (let [point-key  (str x "," y)
        current    (get acc point-key 0)
        next-value (inc current)]
    (assoc acc point-key next-value)))

(defn slope [x1 y1 x2 y2]
  (let [y-diff (- y2 y1)
        x-diff (- x2 x1)]
    (/ y-diff x-diff)))

(defn intercept [m x y]
  (- y (* m x)))

(defn eval-x [m b x]
  (+ b (* m x)))

(defn points-for-diagonal [[x1 y1 x2 y2]]
  (vec
    (if 
      (and (not= x1 x2) (not= y1 y2))
      (let [m  (slope x1 y1 x2 y2)
            b  (intercept m x1 y1)
            xs (range (min x1 x2) (inc (max x1 x2)))]
        (for [x xs] [x (eval-x m b x)]))
      [])))

(defn points-for-horizontal-vertical-line [[x1 y1 x2 y2]]
  (vec 
    (if 
      (or (= x1 x2) (= y1 y2))
      (let [x-min (min x1 x2)
            x-max (max x1 x2)
            y-min (min y1 y2)
            y-max (max y1 y2)]
        (for [x (range x-min (inc x-max))
              y (range y-min (inc y-max))]
          [x y]))
      [])))

(defn format-output [solution]
  (->>
    (vals solution)
    (filter #(<= 2 %))
    (count)))

(defn all-points [xs]
  (vec
    (concat 
      (mapcat points-for-horizontal-vertical-line xs)
      (mapcat points-for-diagonal xs))))

;; Solver Entry Points
(defn solve-part-1 
  [input]
  (->>
    input
    (mapcat points-for-horizontal-vertical-line)
    (reduce tally {})
    (format-output)))

(defn solve-part-2 
  [input]
  (->>
    input
    (all-points)
    (reduce tally {})
    (format-output)))

(defn -main []
  (let [input-file    "resources/input.txt"
        file-contents (slurp input-file)
        input         (convert-input file-contents)]
    (println (solve-part-2 input))))
