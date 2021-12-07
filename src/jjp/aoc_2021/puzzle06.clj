(ns jjp.aoc-2021.puzzle06
  (:require
   [jjp.aoc-2021.utils :refer :all]
   [hashp.core :refer :all]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def demo-input (as-> "3,4,3,1,2" s
                    (str/split s #",")
                    (mapv parse-long s)
                 ))

(def input (as-> (slurp (io/resource "puzzle_input_06.txt")) s
             (str/trim s)
             (str/split s #",")
             (mapv parse-long s)))

(defn evolve [input]
  (flatten (mapv (fn [n]
          (let [m (- n 1)]
            (if (= -1 m)
              [6 8]
              m))) (flatten input)))
  )


(count (loop [input [demo-input]
              turns (range 80)]
  (if (nil? (first turns))
    input
    (let [input (evolve input)]
      (recur input (rest turns))))))
;; => 5934

(time (count (loop [input [input]
              turns (range 80)]
         (if (nil? (first turns))
           input
           (let [input (evolve input)]
             (recur input (rest turns)))))))
;; => 361169


(time (count (loop [input [input]
                    turns (range 256)]
               (if (nil? (first turns))
                 input
                 (let [input (evolve input)]
                   (recur input (rest turns)))))))

(defn growth-for [n turns]
  (count (loop [input [n]
                turns (range turns)]
           (if (nil? (first turns))
             input
             (let [input (evolve input)]
               (recur input (rest turns)))))))


(defn make-tree [n turns]
  {n (growth-for n turns)})

(defn build-tree [turns]
  (->> (range 9)
       (map #(hash-map  % (growth-for % turns)))
       (reduce conj {}))
  )

(def growth-tree-80 (build-tree 80))

(defn score [input tree]
  (->> input
       (map #(get tree %))
       (reduce +))
  )
(score demo-input growth-tree-80)

(score input growth-tree-80)

(def growth-tree-256 (build-tree 256))

(score demo-input growth-tree-256)

 (defn solve [data days]
  (let [start-state (reduce
                     (fn [sofar next]
                       (update sofar next inc))
                     [0 0 0 0 0 0 0 0 0] data)]
    (apply + (loop [i 0
                    gens start-state]
               (if (< i days)
                 (let [last (first gens)
                       gens  (into []  (drop 1 gens))
                       gens  (assoc gens 6 (+ (nth gens 6) last))
                       gens  (conj gens last)]
                   (recur (inc i) gens))
                 gens)))))

(solve input 256)
;; => 1634946868992

(reduce
 (fn [sofar next]
   (update sofar next inc))
 [0 0 0 0 0 0 0 0 0] demo-input)


;;; Every N days, some number of fish are produced
;;; for each of the 9 days (0..8)
;;; calculate the number of fish that would spawn

(defn solve-2 [days input]
  (reduce (fn [generation _]
            (reduce (fn [new-gen [tick fish-count]]                       ; ❸
                      (if (> tick 0)
                        (update new-gen (dec tick) (fnil + 0) fish-count) ; ❹
                        (-> new-gen
                            (update 6 (fnil + 0) fish-count)
                            (update 8 (fnil + 0) fish-count))))           ; ❺
                    {} generation))                                  ; ❷
          (frequencies input)                                        ; ❶
          (range days)))

(reduce + (vals (solve-2 256 demo-input)))
(reduce + (vals (solve-2 256 input)))

(update {-1 0} -1 (fnil + 0) 10 10 )
