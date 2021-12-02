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
;; solution to part 1 - just a sliding window of two
;; => 1184

;; part 2 is sliding window of 3

(let [s1 demo-input
      s2 (rest s1)
      s3 (rest s2)
      sums (map + s1 s2 s3)]
  (count (filter #(> (second %) (first %)) (map vector sums (rest sums)))))
;; => 5

(let [s1 input
      s2 (rest s1)
      s3 (rest s2)
      sums (map + s1 s2 s3)]
  (count (filter #(> (second %) (first %)) (map vector sums (rest sums)))))
;; a solution to part 2 slide over input to build 3-tuples, sum them, then slide over that to map
;; => 1158
