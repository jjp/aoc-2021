(ns jjp.aoc-2021.puzzle07
  (:require
   [jjp.aoc-2021.utils :refer :all]
   [hashp.core :refer :all]
   [clojure.java.io :as io]
   [clojure.string :as str]))


(def demo-input (as-> "16,1,2,0,4,2,7,1,2,14" s
                      (str/split s #",")
                      (mapv parse-long s)))

(def input (as-> (slurp (io/resource "puzzle_input_07.txt")) s
             (str/trim s)
             (str/split s #",")
             (mapv parse-long s)))


(max demo-input)
(frequencies demo-input)
(as-> demo-input s
  (map #(Math/abs (- 2 %)) s)
  (reduce + s)
  )
;; => 37

(apply max input);; => 1986
(count (frequencies input));; => 646

;; with assumption that 2 is the right position - based on story
;; which is wrong - need to calculate
(as-> input s
  (map #(Math/abs (- 2 %)) s)
  (reduce + s))
;; => 464550

(defn score-pos [input pos]
  (as-> input s
    (map #(Math/abs (- pos %)) s)
    (reduce + s)
    )
  )

(defn score-input [input]
  (let [
        positions (range (apply max input))
        score-pos (partial score-pos input)
        scored-positions (reduce (fn [scores pos] (merge scores {pos (score-pos pos)}))
                                 {}
                                 positions)
        lowest-scored-position (->> scored-positions
                                    (sort-by second #(compare %1 %2))
                                    (ffirst))
        score (as-> input s
                (map #(Math/abs (- lowest-scored-position %)) s)
                (reduce + s))]
    [lowest-scored-position score]))

;; solutions for part1
(score-input demo-input);; => [2 37]

(score-input input);; => [329 340052]

;; now onto part2
