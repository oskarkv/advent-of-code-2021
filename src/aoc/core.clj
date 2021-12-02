(ns aoc.core
  (:use oskarkv.utils)
  (:require [clojure.string :as str]))

(defn read-input [n]
  (str/split (slurp (str "input/" n ".txt")) #"\n"))

(def parse-int #(Integer/parseInt %))

(defn num-increases [window-size values]
  (->>$ (partition window-size 1 values)
    (map sum)
    (map < $ (rest $))
    (filter #{true})
    count))

(defn solve-1 []
  (->> (map parse-int (read-input 1))
    (num-increases 3)))

(defn eventual-pos [coll]
  (->>$ (group-by first coll)
    (fmap #(map lastv %))
    (fmap sum)
    (assoc $ :depth (- (:down $) (:up $)))))

(defn eventual-pos-with-aim [coll]
  (reduce (fn [{:keys [forward aim depth] :as m} [dir v]]
            (if (= dir :forward)
              (-> (update m :forward + v)
                (update :depth + (* aim v)))
              (update m :aim (if (= dir :up) - +) v)))
          {:forward 0 :aim 0 :depth 0}
          coll))

(defn solve-2 []
  (->>$ (read-input 2)
    (map #(str/split % #"\s"))
    (map (juxt (comp keyword first) (comp parse-int lastv)))
    eventual-pos-with-aim
    ((juxt :forward :depth))
    (apply *)))
