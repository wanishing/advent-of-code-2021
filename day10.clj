#!/usr/bin/env bb
(ns day10
  (:require
    [clojure.java.io :as io]))


;; Part 1
(def chunks
  (-> "resources/day10-p1"
      io/reader
      line-seq))


(def opening (into (sorted-set) [\( \[ \< \{]))
(def closing (into (sorted-set) [\) \] \> \}]))
(def penalty (zipmap closing [3 25137 57 1197]))
(def valid-exprs (into #{} (map vector opening closing)))


(defn count-errors
  [i chunk stack]
  (if (< i (count chunk))
    (let [curr (get chunk i)
          top (peek stack)]
      (cond
        (contains? valid-exprs [top curr])
        (count-errors (inc i) chunk (pop stack))
        (contains? opening curr)
        (count-errors (inc i) chunk (conj stack curr))
        :else
        [(penalty curr) stack]))
    [0 stack]))


(defn count-syntax-errors
  [chunks]
  (reduce
    +
    (map
      (fn [c]
        (let [[err _] (count-errors 0 c [])]
          err))
      chunks)))


(println "Part 1:" (count-syntax-errors chunks))


;; Part 2
(def scores (zipmap opening [1 4 2 3]))


(defn auto-complete
  [chunks]
  (let [compute-score (fn [s]
                        (loop [i 0
                               score 0]
                          (if (= i (count s))
                            score
                            (recur (inc i)
                                   (+ (* 5 score) (scores (nth s i)))))))
        incomplete (->> chunks
                        (map (fn [c] (count-errors 0 c [])))
                        (filter (fn [[err _]] (zero? err)))
                        (map (fn [[_ stack]]
                               (compute-score (reverse stack)))))
        median (-> incomplete
                   sort
                   (nth (/ (count incomplete) 2)))]
    median))


(println "Part 2:" (auto-complete chunks))


