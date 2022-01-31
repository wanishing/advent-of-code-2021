(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(def edges (->> "resources/day12"
                (io/reader)
                line-seq
                (map (fn [e]
                       (string/split e #"-")))
                (mapcat (fn [[e0 e1]]
                          (if (and (not= e0 "start")
                                   (not= e1 "end"))
                            [[e0 e1] [e1 e0]]
                            [[e0 e1]])))))

(defn small? [n]
  (= n (string/lower-case n)))

(defn count-all-paths [curr visited neighbors]
  (if (= "end" curr)
    1
    (let [valid-neighbors (filter
                           (fn [n]
                             (not
                              (and (small? n)
                                   (contains? visited n))))
                           (get neighbors curr))
          current-count (reduce
                         +
                         (map
                          (fn [n]
                            (count-all-paths n
                                         (conj visited n)
                                         neighbors))
                          valid-neighbors))]
      current-count)))

(defn count-paths [edges]
  (let [neighbors   (reduce-kv (fn [m k v]
                                 (assoc m k (map second v)))
                               {}
                               (group-by first edges))]
    (count-all-paths "start" #{"start"} neighbors)))

(count-paths edges)
