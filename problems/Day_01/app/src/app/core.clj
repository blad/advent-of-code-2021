(ns app.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn zip [xs ys] 
  (map vector xs ys))

(defn increasing? 
  "Determines if a value is increasing"
  [current-value next-value] 
  (> next-value current-value))

(defn solve-part-1 [values]
  (let [offset-values   (drop 1 values)
        collections     (zip values offset-values)
        count-increases (apply + (map #(if (< (first %) (second %)) 1 0) collections))]
    count-increases))

(defn solve-part-2 [values]
  (let [values-position-2 (drop 1 values)
        values-position-3 (drop 1 values-position-2)
        collections     (map vector values values-position-2 values-position-3)
        sums            (map #(apply + %) collections)]
    (solve-part-1 sums)))

(defn -main []
  (let [input-file    "resources/input.txt"
        file-contents (slurp input-file)
        inputs        (map #(Integer/parseInt %) (str/split file-contents #"\n"))]
    ;;; Update the invoked function to toggle between solving part 1 and part 2
    (println (solve-part-2 inputs))))
