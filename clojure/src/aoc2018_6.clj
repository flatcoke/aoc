(ns aoc2018_6
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)


(defn get-sample-data [path]
  (->> path
       (io/resource)
       (slurp)
       (string/split-lines)
       (map (fn [s] (string/split s #", ")))
       (map (fn [[x y]] (vector (Integer. x) (Integer. y))))))

(defn generate-coords-map
  "각 좌표를 이용하여 최소좌표 최대좌표 영역, 경계선을 계산"
  [coords]
  (let [min-x               (->> coords
                                 (map first)
                                 (apply min))
        max-x               (->> coords
                                 (map first)
                                 (apply max))
        min-y               (->> coords
                                 (map second)
                                 (apply min))
        max-y               (->> coords
                                 (map second)
                                 (apply max))
        coord-area          (for [delta-x (range (+ (- max-x min-x) 1))
                                  delta-y (range (+ (- max-y min-y) 1))]
                              [(+ min-x delta-x) (+ min-y delta-y)])
        coord-area-boundary (->> coord-area
                                 (filter (fn [[x y]]
                                           (or (= x min-x)
                                               (= x max-x)
                                               (= y min-y)
                                               (= y max-y)))))]

    {:min-x               min-x
     :min-y               min-y
     :max-x               max-x
     :max-y               max-y
     :coord-area          coord-area
     :coord-area-boundary coord-area-boundary
     :coords              coords}))


(defn distance
  "두 좌표간의 거리를 계산
  input: [0, 0] [1, 1]
  output: 2"
  [[from-x from-y] [to-x to-y]]
  (+ (abs (- from-x to-x)) (abs (- from-y to-y))))



(defn get-closest-distance-coordinates
  "각 영역마다 마다 제일 가까운 좌표를 계산"
  [area coordinates]
  (->> area
       (reduce (fn [acc area]
                 (conj acc {area (map (fn [coord]
                                        [coord (distance area coord)])
                                      coordinates)}))
               {})
       (reduce (fn [acc obj]
                 (let [[f s] (->> (val obj)
                                  (sort-by second)
                                  (take 2))]
                   (conj acc (if (not= (second f) (second s))
                               {(key obj) (first f)} {})))) {})))


(comment
  "day6 part1"
  (let [coord-data          (->> (get-sample-data "aoc2018_6.txt")
                                 generate-coords-map)

        coord-area-boundary (:coord-area-boundary coord-data)
        coord-area          (:coord-area coord-data)
        coords              (:coords coord-data)
        closest_coords      (get-closest-distance-coordinates coord-area coords)
        infinity-coords     (->> (get-closest-distance-coordinates coord-area-boundary coords)
                                 vals
                                 set)]

    (->> closest_coords
         vals
         frequencies
         (sort-by val >)
         (filter (fn [[k _]]
                   (not (contains? infinity-coords k))))
         first)))

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

(comment
  "day6 part2"
  (let [coord-map (->> (get-sample-data "aoc2018_6.txt")
                       generate-coords-map)
        area (:area-boundary coord-map)
        coords (:coords coord-map)]
    (->> area
         (map (fn [xy]
                (apply + (map (fn [compare-xy]
                                (distance xy compare-xy)) coords))))
         (filter #(> 10000 %))
         count)))
