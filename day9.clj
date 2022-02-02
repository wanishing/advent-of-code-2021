#!/usr/bin/env bb
(ns day9
  (:require
    [clojure.string :as str]))


;; Part 1
(def points
  (->> "resources/day9"
       slurp
       (str/split-lines)
       (mapv (fn [line] (mapv #(parse-long (str %)) line)))
       to-array-2d))


(defn saget
  [mx i j]
  (try
    (aget mx i j)
    (catch Exception _)))


(defn adjacents
  [mx i j]
  (let [adjacents [[(dec i), j] [i (inc j)]
                   [(inc i) j] [i (dec j)]]]
    (into #{}
          (filter
            (fn [[k l]]
              (some? (saget mx k l)))
            adjacents))))


(defn low?
  [mx i j adjs]
  (every?
    (fn [[k l]]
      (< (aget mx i j)
         (aget mx k l)))
    adjs))


(defn low-points
  [mx]
  (let [n (count mx)
        m (count (aget mx 0))
        low-points (filter
                     (fn [[i j]]
                       (low? mx i j (adjacents mx i j)))
                     (for [i (range n)
                           j (range m)]
                       [i j]))]
    low-points))


(defn risk-level
  [mx]
  (let [risk-level (reduce
                     +
                     (map
                       (fn [[i j]]
                         (inc (aget mx i j)))
                       (low-points mx)))]
    risk-level))


(println "Part 1:" (risk-level points))


;; Part 2:

(defn find-basin
  [mx i j]
  (let [low-points (filter
                    (fn [[k l]]
                      (< (aget mx i j) (aget mx k l) 9))
                    (adjacents mx i j))
        basin (concat
               low-points
               (mapcat
                (fn [[k l]]
                  (find-basin mx k l))
                low-points))]
    (into #{} basin)))


(defn basins
  [mx]
  (let [basins (map
                 (fn [[i j]]
                   (conj (find-basin mx i j) [i, j]))
                 (low-points mx))

        cnt (->> basins
                 (map count)
                 sort
                 reverse
                 (take 3)
                 (reduce *))]
    cnt))


(println "Part 2:" (basins points))


