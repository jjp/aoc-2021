(ns jjp.aoc-2021.puzzle03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn flip-bits
  "Flip bits in a string. Example:

  (flip-bits \"10001\") => '01110'
  "
  [orig]
  (subs (Long/toBinaryString (bit-not (Long/parseLong orig 2))) (- 64 (count orig))))

(flip-bits "10001")

(def demo-input
  (-> "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
      (str/split #"\n")))

(defn mcv
  "returns the most common value from a string."
  [input]
  (->> (into {} (frequencies input))
       (sort-by second #(compare %2 %1))
       (ffirst)))

(defn pull-bits
  "takes the nth character or bit from a set of strings"
  [coll n]
  (map #(nth % n) coll)
  )

(defn gamma
  "calculate the gamma rate per: https://adventofcode.com/2021/day/3#part1"
  [input]
  (let [x (count (first input))]
  (reduce (fn [c v] (conj c (mcv (pull-bits input v)))) [] (range x))
  ))

(let [gammas (apply str (gamma demo-input))
      epsilons (flip-bits gammas)]
  [gammas epsilons]
  (* (Long/parseLong gammas 2) (Long/parseLong epsilons 2)))
;; => 198

(def input
  (line-seq (io/reader (io/resource "puzzle_input_03.txt"))))

(let [gammas (apply str (gamma input))
      epsilons (flip-bits gammas)]
  [gammas epsilons]
  (* (Long/parseLong gammas 2) (Long/parseLong epsilons 2)))
;; => 2967914
