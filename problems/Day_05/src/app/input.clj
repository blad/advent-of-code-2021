(ns app.input
  (:require [clojure.string :as str])
  (:require [app.utils :refer :all]))

(defn split-at-comma [coords]
  (vec (map #(Integer/parseInt %) (str/split coords #","))))

(defn to-coords [line]
  (->> 
    (str/split line #"->")
    (map str/trim)
    (mapcat split-at-comma)
    (vec)))

(defn split-at-newline 
  [string] 
  (str/split string #"\n"))

(defn convert-input 
  "Convert the file contents to a workable format for solving the problem"
  [file-contents] 
  (let [lines  (split-at-newline file-contents)
        parsed (vec (map to-coords lines))]
    parsed))
