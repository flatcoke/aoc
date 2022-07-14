(ns aoc2018_5
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

(defn get-sample-data [path]
  (->> path
       (io/resource)
       (slurp)))


;; version 1

(defn is-pair-alphabet
  "같은 문자에 대문주 소문자인지를 검사한다.
  case: 1
    input [A a] 
    output true
  case: 2
    intput [a a] 
    output false
  case: 3
    intput [a b] 
    output false"
  [a b]
  (->> (- (int (.charAt a 0)) (int (.charAt b 0)))
       abs
       (= 32)))

(defn is-pair-alphabet?
  "같은 문자에 대문주 소문자인지를 검사한다.
  case: 1
    input [A a] 
    output true
  case: 2
    intput [a a] 
    output false
  case: 3
    intput [a b] 
    output false"
  [a b]
  (->> (- (int a) (int b))
       abs
       (= 32)))

(defn remove-pair
  "마지막 글자와 비교하여 매칭시 제거합니다."
  [trace c]
  (if (and (not-empty trace)
           (is-pair-alphabet? (peek trace) c))
    (pop trace)
    (conj trace c)))

(comment
  "day5 part1"
  (->>
   (get-sample-data "aoc2018_5.txt")
   (reduce remove-pair [])
   count))

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))


(defn remove-specific-word-in-sentence
  [sentence  word]
  (-> (string/replace sentence (str word) "")
      (string/replace (string/upper-case (str word)) "")))

(comment
  "day5 part2"
  (let [input-data    (get-sample-data "aoc2018_5.txt")
        chars         (char-range \a \z)
        count-results (map (fn [c]
                             c
                             (->> (remove-specific-word-in-sentence input-data c)
                                  (reduce remove-pair [])
                                  count)) chars)
        min-count     (apply min count-results)]
    min-count))