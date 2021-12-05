(ns aoc.core
  (:use oskarkv.utils)
  (:require [clojure.string :as str]))

(defn read-raw-input [n]
  (slurp (str "input/" n ".txt")))

(defn read-input [n]
  (str/split (read-raw-input n) #"\n"))

(defn parse-int
  ([x] (Integer/parseInt x))
  ([x radix] (Integer/parseInt x radix)))

(defn transpose [m]
  (vec (apply zip m)))

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

(defn to-digits [bin-string]
  (mapv (comp parse-int str) bin-string))

(defn digits-to-decimal [digits]
  (parse-int (apply str digits) 2))

(defn most-common [coll]
  (if (apply >= (map #(count (filter #{%} coll)) [1 0])) 1 0))

(def least-common (comp #(- 1 %) most-common))

(defn find-by-criterion [crit]
  (fn f [coll]
    (cond (== 1 (count coll)) (first coll)
          (and (seq coll) (ffirst coll))
          (let [x (crit (map first coll))]
            (cons x (f (map rest (filter (comp #{x} first) coll))))))))

(defmacro day-3-solver {:style/indent 1} [name & body]
  `(defn ~name []
     (->>$ (read-input 3)
       (map to-digits)
       ~@body
       (map digits-to-decimal)
       (apply *))))

(day-3-solver solve-3-1
  transpose
  ((juxt #(map most-common $) #(map least-common $))))

(day-3-solver solve-3-2
  ((apply juxt (map find-by-criterion [most-common least-common]))))

(defn winner? [drawn board]
  (let [rows (partition 5 board)
        cols (transpose rows)]
    (when (some identity (map #(every? (set drawn) %) (concat rows cols)))
      [drawn board])))

(defn winners [drawn boards]
  ((fn f [n left]
     (let [w? #(winner? (take n drawn) %)]
       (when (seq left)
         (lazy-cat (keep w? left) (f (inc n) (remove w? left))))))
   5 boards))

(defn score [[drawn board]]
  (* (last drawn) (sum (remove (set drawn) board))))

(defn solve-4 []
  (let [[drawn boards] (str/split (read-raw-input 4) #"\n" 2)
        drawn (map parse-int (str/split drawn #","))
        boards (->> (str/split (str/trim boards) #"\s+")
                 (map parse-int)
                 (partition 25))]
    (map score ((juxt first last) (winners drawn boards)))))
