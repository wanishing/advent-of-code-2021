#!/usr/bin/env bb
(ns day10
  (:require [clojure.java.io :as io]))


(def chunks (-> "resources/day10"
                 io/reader
                 line-seq))

(def opening (into (sorted-set) [\( \[ \< \{]))
(def closing (into (sorted-set) [\) \] \> \}]))
(def penalty (zipmap closing [3 25137 57 1197]))
(def valid-exprs (into #{} (map vector opening closing)))

(defn count-errors [i chunk stack]
  (if (< i (count chunk))
    (let [curr (get chunk i)
          top (peek stack)]
      (cond
        (contains? valid-exprs [top curr])
        (count-errors (inc i) chunk (pop stack))
        (contains? opening curr)
        (count-errors (inc i) chunk (conj stack curr))
        :else
        (penalty curr)))
    0))

(defn count-syntax-errors [chunks]
  (reduce
   +
   (map
    (fn [c]
      (count-errors 0 c []))
    chunks)))

(count-syntax-errors chunks)
