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
 
;; Functions for updating state w.r.t. part 1
(defn up-part-1 [state delta] 
  (update-state state :vertical (- delta)))

(defn down-part-1 [state delta] 
  (update-state state :vertical delta))

(defn forward-part-1 [state delta] 
  (update-state state :horizontal delta))

(defn tally-part-1 [state [direction value]] 
  (cond 
    (= :up direction) (up-part-1 state value)
    (= :down direction) (down-part-1 state value)
    (= :forward direction) (forward-part-1 state value)
    :else state))

;; Functions for updating state w.r.t. part 2
(defn up-part-2 [state delta] 
  (update-state state :aim (- delta)))

(defn down-part-2 [state delta] 
  (update-state state :aim delta))

(defn compute-depth-delta [{aim :aim} delta] 
  (* aim delta))

(defn forward-part-2 [state delta] 
  (let [depth-delta (compute-depth-delta state delta)]
    (-> state
      (update-state :horizontal delta)
      (update-state :vertical depth-delta))))

(defn tally-part-2 [state [direction value]] 
  (cond 
    (= :up direction) (up-part-2 state value)
    (= :down direction) (down-part-2 state value)
    (= :forward direction) (forward-part-2 state value)
    :else state))

;; Solver Functions
(defn compute-output-value [{horizontal :horizontal vertical :vertical}]
  (* horizontal vertical))

(defn solve-part-1
  "Solve Part 1 of Day 2" 
  [values]
  (compute-output-value
    (reduce tally-part-1 {:horizontal 0 :vertical 0} values)))

(defn solve-part-2
  "Solve Part 2 of Day 2" 
  [values]
  (compute-output-value
    (reduce tally-part-2 {:horizontal 0 :vertical 0 :aim 0} values)))

(defn -main []
  (let [input-file    "resources/input.txt"
        file-contents (slurp input-file)
        inputs        (convert-input file-contents)]
    (println (solve-part-2 inputs))))
