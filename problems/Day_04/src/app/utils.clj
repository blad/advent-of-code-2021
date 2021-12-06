(ns app.utils
  (:require [clojure.string :as str]))

(defn transpose 
  "Returns the transpose of the collection of collections."
  [xxs]
  (apply map vector xxs))

(defn contains-value? [col value]
  (some #(= value %) col))

(defn debug [value]
  (do
    (println value)
    value))

(defn to-decimal 
  [string] 
  (Integer/parseInt (str/trim string)))
