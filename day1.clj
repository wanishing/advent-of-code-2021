#!/usr/bin/env bb
(ns day1
  (:require
    [clojure.java.io :as io]))


;; Part 1
(def ms
  (->> "resources/day1-p1"
       io/reader
       line-seq
       (map (fn [m] (Integer/parseInt m)))))


(defn count-increases
  [ms]
  (loop [i 1
         acc 0]
    (if (< i (count ms))
      (recur (inc i)
             (if (> (nth ms i) (nth ms (dec i)))
               (inc acc)
               acc))
      acc)))


(println "Part 1:" (count-increases ms))


;; Part 2

(defn count-window-increases
  [ms]
  (let [sum (fn [m]
              (reduce + m))
        windows (->> ms
                     (partition 3 1)
                     (map sum))
        result (count-increases windows)]
    result))


(println "Part 2:" (count-window-increases ms))
