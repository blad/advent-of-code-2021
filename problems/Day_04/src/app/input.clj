(ns app.input
  (:require [clojure.string :as str])
  (:require [app.utils :refer :all]))

(defn split-at-newline 
  [string] 
  (str/split string #"\n"))

(defn keep-non-empty [xs]
  (vec (filter #(not= 0 (.length %)) xs)))

(defn convert-line-to-row
  [line] 
  (map str/trim (str/split (str/trim line) #"\s+")))

(defn to-boards 
  [rows] 
  (partition 5 rows))

(defn convert-input 
  "Convert the file contents to a workable format for solving the problem"
  [file-contents] 
  (let [split-input            (split-at-newline file-contents)
        [numbers & board-rows] (keep-non-empty split-input)
        bingo-numbers          (str/split numbers #",")
        bingo-boards           (->> board-rows 
                                    (map convert-line-to-row)
                                    (to-boards))]
    {:numbers           bingo-numbers
     :boards            bingo-boards}))
