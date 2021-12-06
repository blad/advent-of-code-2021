(ns app.core
  (:require [clojure.string :as str])
  (:require [app.input :refer :all]) 
  (:require [app.utils :refer :all]) 
  (:gen-class))

(defn bingo-combinations 
  "Returns sequences of possibly winning number sequences."
  [xs]
  (->>
    (range 5 (inc (count xs)))
    (map #(vec (take % xs)))))

(defn row-winner? [candidate row]
  (->> 
    row 
    (map #(contains-value? (vec candidate) %))
    (every? true?)))

(defn single-board-winner? [candidate board]
  (->> board
    (map #(row-winner? candidate %))
    (some true?)))

(defn board-winner? [candidate board]
  (->> [board (transpose board)]
    (map #(single-board-winner? candidate %))
    (some true?)))

(defn keep-winners 
  [{candidate :candidate boards :boards}]
  (let [winning-boards (->> 
                         boards
                         (filter #(board-winner? candidate %))
                         (vec))]
    (if (not-empty winning-boards)
      [{:numbers candidate :boards winning-boards}]
      [])))

(defn solution-board-combo 
  [boards candidates]
  (map #(hash-map :candidate % :boards boards) candidates))

(defn find-first-winner
  [{numbers :numbers
    boards  :boards}]
  (->>
    (bingo-combinations numbers)
    (solution-board-combo boards)
    (mapcat keep-winners)
    (first)))
        
(defn format-output [{numbers :numbers winning-board :boards}]
  (let [winning-number   (to-decimal (last numbers))
        board-numbers    (vec (flatten winning-board))
        uncalled-numbers (->> board-numbers
                           (filter #(not (contains-value? numbers %)))
                           (map to-decimal))]
    (* winning-number (apply + uncalled-numbers))))

;; Solver Entry Points
(defn solve-part-1 
  [input]
  (->>
    input
    (find-first-winner)
    (format-output)))
        
(defn -main []
  (let [input-file    "resources/sample_input.txt"
        file-contents (slurp input-file)
        input        (convert-input file-contents)]
    (println (solve-part-1 input))))
