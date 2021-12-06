(ns advent2021.day-01.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn read-numbers-from-file [file-name]
  (map #(Integer/parseInt %)
       (line-seq (io/reader file-name))))

(defn part1 [file-name]
  (loop [numbers (read-numbers-from-file file-name)
         num-increments 0]
    (let [[x & rest] numbers
          y (first rest)]
      (if (and (number? x) (number? y))
        (recur rest
               (if (> y x)
                 (inc num-increments)
                 num-increments))
        num-increments))))

;; After I wrote the above I read
;; https://github.com/tonsky/advent-of-code/blob/main/src/advent_of_code/year2021/day1.clj
;; and realized I wasn't really taking advantage of Clojure seqs.
;; Seems obvious in hindsight.  I'm sure this is still not the best,
;; but I think it's a vast improvement over part1.
(defn part2 [file-name]
  (let [numbers (read-numbers-from-file file-name)
        sums (map + numbers (nthrest numbers 1) (nthrest numbers 2))]
    (count (filter pos? (map - (next sums) sums)))))

(defn -main [& args]
  (println "Part 1: " (part1 (first args)))
  (println "Part 2: " (part2 (first args))))
