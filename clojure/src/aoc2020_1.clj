(ns aoc2020_1
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))
;; # Day 1

;; [https://adventofcode.com/2020/day/1](https://adventofcode.com/2020/day/1)

;; ## 파트 1
;; 더해서 2020이 되는 두 숫자의 곱을 구하시오. (두 숫자는 유일하다고 가정)

;; 예) 1721 979 366 299 675 1456 의 경우 1721 * 299 = 514579 를 출력

(defn get-sample-data [path]
  (->> (io/resource path)
       (slurp)
       (string/split-lines)
       (map #(Integer/parseInt %))))

(comment
  (let [input  (get-sample-data "aoc2020_1.txt")]
    (->> (map (fn [x] (map (fn [y] [x y]) input)) input)
         (apply concat)
         (filter (fn [[x y]] (= (+ x y) 2020)))
         first
         (apply *))))

;; ## 파트 2
;; 같은 입력이 주어질 때, 더해서 2020이 되는 세 숫자의 합을 구하시오.

;; 예) 2020 = 979 + 366 + 675, 곱하면 241861950 을 출력




