(ns aoc.core
  (:use oskarkv.utils)
  (:require [clojure.string :as str]))

(def parse-int #(Integer/parseInt %))

(defn num-increases [window-size values]
  (->>$ (partition window-size 1 values)
    (map sum)
    (map < $ (rest $))
    (filter #{true})
    count))

(defn solve-1 []
  (->> (map parse-int (str/split (slurp "input/1.txt") #"\n"))
    (num-increases 3)))
