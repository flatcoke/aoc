(ns aoc2020_8
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; # Day 8

;; [https://adventofcode.com/2020/day/8](https://adventofcode.com/2020/day/8)

;; ## 파트 1
;; 일련의 지시가 입력으로 주어진다.
;; - **acc**는 전역 변수를 증가/감소 시키는 역할을 한다. acc +7은 accumulator를 7 증가 시킨다. accumulator는 0에서 시작한다.
;; - **jmp**는 현재 위치에 기반하여 새로운 지시로 넘어간다. jmp +1은 바로 다음의 지시로 넘어가는 것이고, jmp +2는 바로 다음의 지시는 건너뛰고 그 다음의 지시를 실행하는 것이다.
;; - **nop** 는 아무것도 하지 않는다.
;;   아래는 예시이다.
;; ```
;; nop +0
;; acc +1
;; jmp +4
;; acc +3
;; jmp -3
;; acc -99
;; acc +1
;; jmp -4
;; acc +6
;; ```
;; 위의 예시는 아래의 순서로 실행된다.
;; ```
;; nop +0  | 1
;; acc +1  | 2, 8(!)
;; jmp +4  | 3
;; acc +3  | 6
;; jmp -3  | 7
;; acc -99 |
;; acc +1  | 4
;; jmp -4  | 5
;; acc +6  |
;; ```
;; 이 지시들은 무한히 반복된다.

(defn get-sample-data [path]
  (->> path
       (io/resource)
       (slurp)
       (string/split-lines)
       (map-indexed (fn [index line]
                      (let [[action value] (string/split line #" ")]
                        [index action (Integer/parseInt value)])))))

(defn get-acc-before-duplicated-trace
  "반복 순환 구조 전까지 계산된 값을 반환합니다."
  [input]
  (loop [i     0
         acc   0
         trace #{}]

    (let [[index action value] (nth input i)]
      (if (contains? trace index)
        acc
        (cond
          (= action "acc")
          (recur (inc i) (+ acc value) (conj trace index))
          (= action "jmp")
          (recur (+ i value) acc (conj trace index))
          (= action "nop")
          (recur (inc i) acc (conj trace index)))))))

(comment
  (->> (into [] (get-sample-data "aoc2020_8.txt"))
       (get-acc-before-duplicated-trace)))



;; 한 지시가 정확히 **두번 실행되는 시점 바로 전**의 acc의 값을 반환하라.
;; 위의 예시에선 acc +1이 8번째 틱에서 정확히 두번 실행되고, 이 때의 acc의 값은 5이다.



;; ## 파트 2
;; 주어진 지시들 중, 정확히 하나의 지시가 잘못된 것을 알게 되었다.
;; 정확히 하나의 jmp가 nop가 되어야하거나, nop가 jmp가 되면 프로그램은 **종료**된다.

;; ```
;; nop +0  | 1
;; acc +1  | 2
;; jmp +4  | 3
;; acc +3  |
;; jmp -3  |
;; acc -99 |
;; acc +1  | 4
;; nop -4  | 5 ;; 여기!
;; acc +6  | 6
;; ```

;; 위의 예시에서, "여기!" 라고 표기된 곳이 jmp에서 nop로 바뀌면, 지시는 무한히 반복하지 않고 마지막에 6을 반환하며 종료된다.
;; 프로그램이 종료되는 시점의 accumulator의 값을 반환하여라.