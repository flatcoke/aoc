(ns aoc2018_3
  (:require [clojure.set :as set]
            [clojure.string :as string]))


;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)


(defn get-sample-data [path]
  (->> (slurp path)
       (string/split-lines)))

(defn clean-up-dirty-word
  "치환할때 필요없는 데이터를 삭제
  input: '#3 @ 5,5: 2x2'
  output: '3 5,5 2x2'"

  [dirty-str]
  (-> (string/replace dirty-str "@" "")
      (string/replace "#" "")
      (string/replace ":" "")
      (string/replace "  " " ")))

(defn parse-string-to-location-map
  "FROM `1 1,3 4x4` TO {:id :x :y :width :height} 형태로 치환"
  [s]
  (let [data           (string/split s #" ")
        id             (Integer/parseInt (data 0))
        [x y]          (map #(Integer/parseInt %) (string/split (data 1) #","))
        [width height] (map #(Integer/parseInt %) (string/split (data 2) #"x"))]
    {:id     id
     :x      x
     :y      y
     :width  width
     :height height}))

(defn generate-map-key
  "key로 사용하기 위해 x y를 콤마로 구분한 문자로 변경
  input: 10 20
  output: '10,20'"
  [x y]
  (str x "," y))


(defn generate-range-coordinate
  "location 정보를 받아 해당하는 모든 좌표 문자로 치환
    input: {:x 0 :y 0 :width 2 :height 2}
    output: `(
                0,0 
                0,1
                1,0
                1,1
             )"
  [location-map]
  (let [x (get location-map :x)
        y (get location-map :y)
        w (get location-map :width)
        h (get location-map :height)]

    (for [xx (range w)
          yy (range h)]
      (generate-map-key (+ x xx) (+ y yy)))))

(comment
  "day3 part1"
  (->> (get-sample-data "./resources/aoc2018_3.txt")
       (map clean-up-dirty-word)
       (map parse-string-to-location-map)
       (map generate-range-coordinate)
       (flatten)
       (frequencies)
       (vals)
       (filter #(> % 1))
       (count)))


;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)


(comment
  "day3 part2"
  (let [all-location-map (->> (get-sample-data "./resources/aoc2018_3.txt")
                              (map clean-up-dirty-word)
                              (map parse-string-to-location-map))
        only-went-one    (->> all-location-map
                              (map generate-range-coordinate)
                              (flatten)
                              (frequencies)
                              (into {} (filter #(-> % val (= 1))))
                              (keys)
                              (set))]
    (->> all-location-map
         (map #(list (set (generate-range-coordinate %)) (get % :id)))
         (filter #(empty? (set/difference (first %) only-went-one)))
         (first)
         (last))))
