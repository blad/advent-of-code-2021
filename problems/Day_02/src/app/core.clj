(ns app.core
  (:require [clojure.string :as str])
  (:gen-class))

;; Functions for converting input
(defn convert-line
  "Converts the line into a direction and value"
  [line] 
  (let [[direction value] (str/split line #" ")
        direction-key     (keyword direction)
        value-int         (Integer/parseInt value)]
    [direction-key value-int]))

(defn convert-input 
  "Convert the file contents to a workable format for solving the problem"
  [file-contents] 
  (map 
    convert-line
    (str/split file-contents #"\n")))

;; Functions for updating state
(defn update-state [state direction delta] 
   (let [current-value (get state direction 0)]
    (assoc state direction (+ current-value delta))))
 
(defn up [state delta] 
  (update-state state :vertical (- delta)))

(defn down [state delta] 
  (update-state state :vertical delta))

(defn forward [state delta] 
  (update-state state :horizontal delta))

(defn tally [state [direction value]] 
  (cond 
    (= :up direction) (up state value)
    (= :down direction) (down state value)
    (= :forward direction) (forward state value)
    :else state))

;; Solver Functions
(defn compute-output-value [{horizontal :horizontal vertical :vertical}]
  (* horizontal vertical))

(defn solve-part-1
  "Solve Part 1 of Day 2" 
  [values]
  (compute-output-value
    (reduce tally {:horizontal 0 :vertical 0} values)))

(defn -main []
  (let [input-file    "resources/input.txt"
        file-contents (slurp input-file)
        inputs        (convert-input file-contents)]
    (println (solve-part-1 inputs))))
