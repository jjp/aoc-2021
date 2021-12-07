(ns jjp.aoc-2021.puzzle05
  (:require
   [hashp.core :refer :all]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-long [s]
  (Long/parseLong s))

(defn parse-coordinates [line]
  (let [[_ x1 y1 x2 y2]
        (re-matches #"(\d+),(\d+)\s+->\s+(\d+),(\d+)" line)]
    [[(parse-long x1) (parse-long y1)]
     [(parse-long x2) (parse-long y2)]]))

(def demo-input
  (as-> "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2" s
    (str/split-lines s)
    (map parse-coordinates s)
    ))

(map #(parse-coordinates %) (str/split-lines demo-input))

(defn build-line [[[x1 y1] [x2 y2]]]
  (let [[x1 x2] (sort [x1 x2])
        [y1 y2] (sort [y1 y2])]
    (for [x (range x1 (inc x2))
          y (range y1 (inc y2))]
      [x y])))

(defn keep-non-diagonal [coordinates]
  (filter (fn [[[x1 y1] [x2 y2]]]
            (or (= x1 x2) (= y1 y2)))
          coordinates))

(defn mark-line [field line]
  (let [fr (frequencies line)]
    (merge-with (fnil + 1) field fr)))

(defn mark-lines [field lines]
  (reduce mark-line field lines))

(defn create-field [input]
  (->> input
       keep-non-diagonal
       (map build-line)
       (mark-lines {})))

(defn part-1 [input]
  (->> (create-field input)
       vals
       (filter #(> % 1))
       count))

(defn read-input []
  (->> (io/resource "puzzle_input_05.txt")
       slurp
       str/split-lines
       (map parse-coordinates)))

(part-1 demo-input)
;; => 5

(read-input)
(part-1 (read-input))
;; => 7380

(defn build-diagonal-line [[[x1 y1] [x2 y2]]]
  (when (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))
    (let [xdir (if (> 0 (- x2 x1)) -1 1)
          ydir (if (> 0 (- y2 y1)) -1 1)]
      (loop [line [[x1 y1]]]
        (let [[x y] (last line)]
          (if (and (not= x x2) (not= y y2))
            (recur (conj line [(+ x xdir) (+ y ydir)]))
            line))))))

(defn part-2 [input]
  (->> input
       (keep build-diagonal-line)
       (mark-lines (create-field input))
       vals
       (filter #(> % 1))
       count))

(part-2 demo-input)
;; => 12

(time (part-2 (read-input)))
;; => 21373
