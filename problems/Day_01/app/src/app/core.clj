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

(defn -main []
  (let [input-file    "resources/input.txt"
        file-contents (slurp input-file)
        inputs        (map #(Integer/parseInt %) (str/split file-contents #"\n"))]
    (println (solve-part-1 inputs))))
