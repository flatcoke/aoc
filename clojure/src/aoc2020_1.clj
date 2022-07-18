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
  "2020 day1 part1
   map을 돌면서 2가지로 조합할 수 있는 모든 경우의 수를 list로 만든 후 
   해당 값이 곱이 2020인 경우를 필터링 
   "
  (let [input  (get-sample-data "aoc2020_1.txt")]
    (->> (map (fn [x] (map (fn [y] [x y]) input)) input)
         (apply concat)
         (filter (fn [[x y]] (= (+ x y) 2020)))
         first
         (apply *))))

;; ## 파트 2
;; 같은 입력이 주어질 때, 더해서 2020이 되는 세 숫자의 합을 구하시오.

;; 예) 2020 = 979 + 366 + 675, 곱하면 241861950 을 출력

(comment
  "2020 day1 part2
   for를 이용하여 3가지 수의 모든 경우를 list로 만들어 
   3개의 곱이 2020인 경우로 필터링 함"
  (let [input  (get-sample-data "aoc2020_1.txt")]
    (->> (for [x input
               y input
               z input]
           (sort [x y z]))
         set
         (filter (fn [[x y z]] (= (+ x y z) 2020)))
         first
         (apply *))))