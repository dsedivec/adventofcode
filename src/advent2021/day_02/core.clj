(ns advent2021.day-02.core
  (:require
   [clojure.java.io :refer [reader]]
   [clojure.string :as string]))

(defn part01
  ([lines] (part01 lines 0 0))
  ([lines depth horiz-pos]
   (if (seq lines)
     (let [[line & lines] lines
           [cmd amt-str] (string/split line #"\s+" 2)
           amt (Integer/parseInt amt-str)]
       (case cmd
         "forward" (recur lines depth (+ horiz-pos amt))
         "down" (recur lines (+ depth amt) horiz-pos)
         "up" (recur lines (- depth amt) horiz-pos)))
     [depth horiz-pos (* depth horiz-pos)])))

(defn part02
  ([lines] (part02 lines 0 0 0))
  ([lines depth horiz-pos aim]
   (if (seq lines)
     (let [[line & lines] lines
           [cmd amt-str] (string/split line #"\s+" 2)
           amt (Integer/parseInt amt-str)]
       (case cmd
         "forward" (recur lines (+ depth (* aim amt)) (+ horiz-pos amt) aim)
         "down" (recur lines depth horiz-pos (+ aim amt))
         "up" (recur lines depth horiz-pos (- aim amt))))
     [depth horiz-pos (* depth horiz-pos)])))

(defn -main [file-name]
  (let [lines (line-seq (reader file-name))]
    (println "Part 1: " (part01 lines))
    (println "Part 2: " (part02 lines))))
