(ns aoc2018-2
  (:require [clojure.string :as string]))

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(defn get-sample-data []
  (->> (slurp "./resources/aoc2018_2.txt")
       (clojure.string/split-lines)))

(comment
  (->> (slurp "./resources/aoc2018_2.txt")
       (clojure.string/split-lines)))


(defn has-duplicated-word? [n word]
  (->> (frequencies word)
       vals
       (filter #(= % n))
       first))

(def has-two-duplicated? (partial has-duplicated-word? 2))

(def has-three-duplicated? (partial has-duplicated-word? 3))

(comment
  (->> (get-sample-data)
       (#(* (count (filter has-two-duplicated? %))
            (count (filter has-three-duplicated? %))))))


;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################


(comment
  (->>
   (range (count "cvgowxquwnhaefmulkjdrptbyi"))
   (println #(subs "cvgowxquwnhaefmulkjdrptbyi" % 1))))

(defn replace-to-dot
  [s n]
  (str (subs s 0 n) "&" (subs s (+ n 1) (count s))))

(comment
  (println (replace-to-dot "taemin" 10))
  (str (subs "taemin" 0 0) (subs "taemin" (+ 0 1) (count "taemin"))))

(comment
  (->>
   (range (count "abcdef"))
   (map #(replace-to-dot "abcdef" %))
   frequencies))

(defn abcd
  [s]
  (->>
   (range (count s))
   (map #(replace-to-dot s %))))

(comment
  (->>
   (get-sample-data)
   (map abcd)
   (flatten)
   frequencies
   (filter #(= (second %) 2))
   (first)
   (first)
  ;;  (clojure.string/replace-first "]" "")))
   ))


(comment
  (clojure.string/replace-first "cvgywxqubnuaefmsl-jdrpfzyi" "-" ""))