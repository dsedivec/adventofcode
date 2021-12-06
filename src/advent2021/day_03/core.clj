(ns advent2021.day-03.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.java.io :as io]))

(defn part01
  ([lines] (part01 lines (vec (repeat (count (first lines)) 0)) 0))
  ([[line & lines] all-num-ones num-lines]
   (let [all-num-ones (map (fn [num-ones ch]
                             (+ num-ones (if (= ch \1) 1 0)))
                           all-num-ones line)
         num-lines (inc num-lines)]
     (if (seq lines)
       (recur lines all-num-ones num-lines)
       (let [midpoint (/ num-lines 2)
             gamma (Integer/parseInt (apply str (map #(if (>= % midpoint) "1" "0")
                                                     all-num-ones))
                                     2)
             epsilon (bit-and-not (dec (math/expt 2 (count all-num-ones))) gamma)]
         [(Integer/toString gamma 2)
          (Integer/toString epsilon 2)
          gamma
          epsilon
          (* gamma epsilon)])))))

(defn -main [file-name]
  (prn "Part 1: " (part01 (line-seq (io/reader file-name)))))
