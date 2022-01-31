(ns day12
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]))


;; Part 1

(defn read-edges
  [f]
  (->> f
       (io/reader)
       line-seq
       (map (fn [e]
              (string/split e #"-")))
       (mapcat (fn [[e0 e1]]
                 (if (and (not= e0 "start")
                          (not= e1 "end"))
                   [[e0 e1] [e1 e0]]
                   [[e0 e1]])))))


(defn neighbors
  [edges]
  (reduce-kv (fn [m k v]
               (assoc m k (map second v)))
             {}
             (group-by first edges)))


(defn small?
  [n]
  (= n (string/lower-case n)))


(defn big?
  [n]
  (not (small? n)))


(defn count-all-paths-p1
  [curr visited neighbors]
  (if (= "end" curr)
    1
    (let [valid-neighbors (filter
                            (fn [n]
                              (or
                                (big? n)
                                (not (contains? visited n))))
                            (get neighbors curr))
          current-count (reduce
                          +
                          (map
                            (fn [n]
                              (count-all-paths-p1 n
                                                  (conj visited n)
                                                  neighbors))
                            valid-neighbors))]
      current-count)))


(defn count-paths-p1
  [edges]
  (count-all-paths-p1 "start" #{"start"} (neighbors edges)))


(println "Part 1:" (count-paths-p1 (read-edges "resources/day12-p1")))


;; Part 2

(defn count-all-paths-p2
  [curr visited neighbors]
  (if (= "end" curr)
    1
    (let [visited-twice (some (fn [[k v]] (and (= v 2) (small? k) k)) visited)
          valid-neighbors (filter
                            (fn [n]
                              (or
                                (big? n)
                                (nil? visited-twice)
                                (not (contains? visited n))))
                            (get neighbors curr))
          current-count (reduce
                          +
                          (map
                            (fn [n]
                              (count-all-paths-p2 n
                                                  (update visited n (fnil inc 0))
                                                  neighbors))
                            valid-neighbors))]
      current-count)))


(defn count-paths-p2
  [edges]
  (let [without-end-start-edges (filter
                                  (fn [[e0 e1]]
                                    (and (not= e1 "start") (not= e0 "end")))
                                  edges)]
    (count-all-paths-p2 "start" {"start" 1} (neighbors without-end-start-edges))))


(println "Part 2:" (count-paths-p2 (read-edges "resources/day12-p1")))
