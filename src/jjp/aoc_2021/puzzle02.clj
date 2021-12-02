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

;; need to go to bed now
;; so, with part 2 instructions, I need to "slide" over the input I think instead of just filtering and
;; summing


(loop [[action & remaining] demo-input
       horizontal 0
       aim 0
       depth 0]
  (let [[command value] (str/split (or action "break 0") #" ")
        value (Long/parseLong value)
        ]
;;     (println {:command command :value value :state {:horizontal horizontal :aim aim :depth depth}})
    (cond (= command "forward") (recur remaining (+ horizontal value) aim (+ depth (* aim value)))
        (= command "down") (recur remaining horizontal (+ aim value) depth)
        (= command "up") (recur remaining horizontal (- aim value) depth)
        :else [horizontal aim depth (* horizontal depth)]
        )))
;; => [15 10 60 900]

(loop [[action & remaining] input
       horizontal 0
       aim 0
       depth 0]
  (let [[command value] (str/split (or action "break 0") #" ")
        value (Long/parseLong value)]
;;     (println {:command command :value value :state {:horizontal horizontal :aim aim :depth depth}})
    (cond (= command "forward") (recur remaining (+ horizontal value) aim (+ depth (* aim value)))
          (= command "down") (recur remaining horizontal (+ aim value) depth)
          (= command "up") (recur remaining horizontal (- aim value) depth)
          :else [horizontal aim depth (* horizontal depth)])))
;; => [2033 750 783289 1592426537]
