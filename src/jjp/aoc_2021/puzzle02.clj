(ns jjp.aoc-2021.puzzle02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def demo-input
  (-> "forward 5
down 5
forward 8
up 3
down 8
forward 2"
      (str/split #"\n"))
  )

(let [horizontal (->> demo-input
                      (map #(str/split % #" "))
                      (filter #(= (first %) "forward"))
                      (map last)
                      (map #(Long/parseLong %))
                      (reduce +))
      down            (->> demo-input
                           (map #(str/split % #" "))
                           (filter #(= (first %) "down"))
                           (map last)
                           (map #(Long/parseLong %))
                           (reduce +))
      up              (->> demo-input
                           (map #(str/split % #" "))
                           (filter #(= (first %) "up"))
                           (map last)
                           (map #(- (Long/parseLong %)))
                           (reduce +))
      depth      (+ down up)
      position   (* horizontal depth)]
  [horizontal depth position])
;; => [15 10 150]


(def input
  (line-seq (io/reader (io/resource "puzzle_input_02.txt"))))

(let [horizontal (->> input
                      (map #(str/split % #" "))
                      (filter #(= (first %) "forward"))
                      (map last)
                      (map #(Long/parseLong %))
                      (reduce +))
      down            (->> input
                           (map #(str/split % #" "))
                           (filter #(= (first %) "down"))
                           (map last)
                           (map #(Long/parseLong %))
                           (reduce +))
      up              (->> input
                           (map #(str/split % #" "))
                           (filter #(= (first %) "up"))
                           (map last)
                           (map #(- (Long/parseLong %)))
                           (reduce +))
      depth      (+ down up)
      position   (* horizontal depth)]
  [horizontal up down depth position])
;; => [2033 -1125 1875 750 1524750]
