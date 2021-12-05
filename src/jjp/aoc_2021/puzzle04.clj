(ns jjp.aoc-2021.puzzle04
  (:require
   [hashp.core :refer :all]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def demo-input
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"
  )

(def input (slurp (io/resource "puzzle_input_04.txt")))

(defn rolls [input]
  (as-> input s
    (str/split s #"\n")
    (first s)
    (str/split s #",")
    (map #(Long/parseLong %) s)))

(defn make-board [rows]
  (into [] (as-> (str/split rows #"\n") s
    (map #(str/split (str/trim %) #"\s+") s)
    (map #(into [] (map (fn [x] [(Long/parseLong x) 0]) %)) s)
    ))
  )

(defn boards [input]
  (as-> input s
    (str/split s #"\n\n")
    (rest s)
    (map #(make-board %) s))
  )

(defn build-game [input]
  (let [rolls (rolls input)
        boards (vec (boards input))]
    [rolls boards]))

(defn mark-row [row roll]
  (map #(if (= (second %) roll)
          [(first %) 1]
          %) row)
  )

(defn mark-board [board roll]
  (map #(if (= (first %) roll)
          [(first %) 1]
          %) board)
)

(defn mark-boards [boards roll]
  (map #(mark-board % roll) boards)
  )

(defn winning-row?
  "a row is a winner if all vals are 1"
  [row]
  (if (= (count row) (count (filter #(= 1 (second %)) row)))
    (first row)
    nil)
  )

(defn winner? [board]
  (or (->> board
           (filter #(winning-row? %))
           (first)
           )
      (->> (apply mapv vector board)
           (filter #(winning-row? %))
           (first)
           )))

(defn game-part1 [input]
  (let [game-board (build-game input)
        rolls (first game-board)
        boards (second game-board)]
    (loop [rolls rolls
           boards boards]
      (let [roll (first rolls)
            boards (map #(mark-boards % roll) boards)
            winner (first (filter winner? boards))]
        (if (or winner (nil? roll))
          {:roll roll :winning-card winner}
          (recur (rest rolls) boards)
          )
        )
      )
    )
  )


(defn score-game [game input]
  (let [{:keys [roll winning-card]} (game input)
        unmarked-cells-sum (->> winning-card
                           (flatten)
                           (partition 2)
                           (filter #(= 0 (second %)))
                           (flatten)  ;; cheat - will have those zeros
                           (reduce +)
                           )
        ]
    (* roll unmarked-cells-sum)
    ))

(score-game game-part1 input)
;; => 51776

;;; answer for part one above!


(defn game-part2 [input]
  (let [game-board (build-game input)
        rolls (first game-board)
        boards (second game-board)]
    (loop [rolls rolls
           boards boards
           last-winner {:roll nil :winning-card nil}]
      (let [roll (first rolls)
            boards (map #(mark-boards % roll) boards)
            winners (filter winner? boards)
            boards  (filter #(not (winner? %)) boards)
            stats #p {:roll roll :boards (count boards) :winners (count winners) :last-roll (:roll last-winner)}

            winner (first winners)
            last-winner (if (nil? winner)
                          last-winner
                          {:roll roll :winning-card winner}
                          )]
        (if (nil? roll)
          last-winner
          (recur (rest rolls) boards last-winner))))))

(game-part2 demo-input)

(score-game game-part2 demo-input)
;; => 1924

(score-game game-part2 input)
;; => 16830

;;; that was an ugly completion - took about 3 hours
;;; tripped up because I kept recounting winning boards, I had
;;; to strip them out after they won
