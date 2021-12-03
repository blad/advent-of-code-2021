(ns app.core
  (:require [clojure.string :as str])
  (:gen-class))

;; Functions for converting input
(defn convert-line
  "Converts the line into a direction and value"
  [line] 
  (map #(Integer/parseInt %) (str/split line #"")))

(defn convert-input 
  "Convert the file contents to a workable format for solving the problem"
  [file-contents] 
  (map 
    convert-line
    (str/split file-contents #"\n")))

(defn tally-update [s x] 
  (if (= 1 x) (inc s) (dec s)))

(defn tally-digits [state row] 
  (map tally-update state row))

(defn invert [xs] 
  (map #(if (= 1 %) 0 1) xs))

(defn exp [base power]
  (int (Math/pow base power)))

(defn to-decimal [x] 
  (let [x-converted (map #(* %1 (exp 2 %2)) x (range))]
    (apply + x-converted)))

(defn compute-output-value [x] 
  (let [x-binary        (map #(if (> 0 %) 1 0) x)
        gamma           (reverse x-binary)
        epsilon         (invert gamma)]
    (* (to-decimal gamma) (to-decimal epsilon))))

;; Solver Functions
(defn solve-part-1
  "Solve Part 1 of Day 3" 
  [values]
  (compute-output-value
    (reduce tally-digits (repeat 0) values)))

(defn -main []
  (let [input-file    "resources/input.txt"
        file-contents (slurp input-file)
        inputs        (convert-input file-contents)]
    (println (solve-part-1 inputs))))
