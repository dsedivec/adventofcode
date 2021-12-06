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

(defn part02
  ([nums cmp-type]
   (part02 nums
           cmp-type
           (bit-shift-left 1 (dec (long (Math/ceil (/ (Math/log (apply max nums))
                                                      (Math/log 2))))))))
  ([nums cmp-type bit]
   (assert (>= bit 1) (format "nums=%s bit=%s" nums bit))
   (println (format "num nums %d bit=%s" (count nums) bit))
   (when (<= (count nums) 8) (println (format "nums=%s" nums)))
   (let [filtered-nums (loop [nums nums
                              zeroes []
                              ones []]
                         (if (seq nums)
                           (let [num (first nums)
                                 is-one (bit-and num bit)]
                             (if (zero? is-one)
                               (recur (next nums) (conj zeroes num) ones)
                               (recur (next nums) zeroes (conj ones num))))
                           (let [cmp ((case cmp-type :oxygen + -)
                                      (compare (count zeroes) (count ones)))]
                             (println (format "%s cmp=%s len zeroes=%d ones=%d"
                                              cmp-type cmp
                                              (count zeroes) (count ones)))
                             (if (zero? cmp)
                               (case cmp-type
                                 :oxygen ones
                                 zeroes)
                               (if (pos? cmp) zeroes ones)))))]
     (if (= (count filtered-nums) 1)
       (first filtered-nums)
       (recur filtered-nums cmp-type (bit-shift-right bit 1))))))

(defn -main [file-name]
  (let [lines (line-seq (io/reader file-name))
        numbers (map #(Integer/parseInt % 2) lines)]
    (println "Part 1: " (part01 lines))
    (let [oxygen (part02 numbers :oxygen)
          co2 (part02 numbers :co2)]
      (println "Part 2: " oxygen co2 (* oxygen co2)))))
