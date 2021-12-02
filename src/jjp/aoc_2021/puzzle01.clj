(ns jjp.aoc-2021.puzzle01
  (:require [clojure.java.io :as io]))


(def demo-input [199
                 200
                 208
                 210
                 200
                 207
                 240
                 269
                 260
                 263])

(count (filter #(> (second %) (first %)) (map vector demo-input (rest demo-input))))
;; => ([199 200]
;;     [200 208]
;;     [208 210]
;;     [210 200]
;;     [200 207]
;;     [207 240]
;;     [240 269]
;;     [269 260]
;;     [260 263])

(def input
  (map #(Long/parseLong %) (line-seq (io/reader (io/resource "puzzle_input_01.txt")))))

(count (filter #(> (second %) (first %)) (map vector input (rest input))))
;; => 1184
