(ns app.core
  (:require [clojure.string :as str])
  (:gen-class))

;; Functions for converting input
(defn convert-line
  "Converts the line into a direction and value"
  [line] 
  (vec (map #(Integer/parseInt %) (str/split line #""))))

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

(defn partition-at 
  "Returns a partiion by zero and ones" 
  [n xs] 
  (let [groups (group-by #(get % n) xs)]
    [(get groups 1) (get groups 0)]))

(defn partition-oxygen-gen [input]
  (loop [xs input
         n  5]
    (if 
      (= 1 (count xs))
      (first xs)
      (let [[os zs] (partition-at (- 5 n) xs)
            os-len  (count os)
            zs-len  (count zs)]
        (recur 
          (if (>= os-len zs-len) os zs)
          (dec n))))))

(defn partition-carbon-scrub [input]
  (loop [xs input
         n  5]
    (if 
      (= 1 (count xs))
      (first xs)
      (let [[os zs] (partition-at (- 5 n) xs)
            os-len  (count os)
            zs-len  (count zs)]
        (recur 
          (if (< os-len zs-len) os zs)
          (dec n))))))

;; Solver Functions
(defn solve-part-1
  "Solve Part 1 of Day 3" 
  [values]
  (compute-output-value
    (reduce tally-digits (repeat 0) values)))

(defn solve-part-2
  "Solve Part 2 of Day 3" 
  [values]
  (let [oxygen (partition-oxygen-gen values)
        carbon (partition-carbon-scrub values)]
    (* (to-decimal (reverse oxygen)) (to-decimal (reverse carbon)))))

(defn -main []
  (let [input-file    "resources/input.txt"
        file-contents (slurp input-file)
        inputs        (convert-input file-contents)]
    (println (solve-part-2 inputs))))
