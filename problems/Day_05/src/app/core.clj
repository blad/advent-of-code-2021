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

(defn points-for-line [[x1 y1 x2 y2]]
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

;; Solver Entry Points
(defn solve-part-1 
  [input]
  (->>
    input
    (mapcat points-for-line)
    (reduce tally {})
    (format-output)))

(defn -main []
  (let [input-file    "resources/input.txt"
        file-contents (slurp input-file)
        input        (convert-input file-contents)]
    (println (solve-part-1 input))))
