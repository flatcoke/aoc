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
    ;;    (map (fn [s] s))))

(defn generate-coords-map
  [coords]
  (let [min-x         (->> coords
                           (map first)
                           (apply min))
        max-x         (->> coords
                           (map first)
                           (apply max))
        min-y         (->> coords
                           (map second)
                           (apply min))
        max-y         (->> coords
                           (map second)
                           (apply max))
        area-boundary (for [delta-x (range (- max-x min-x))
                            delta-y (range (- max-y min-y))]
                        [(+ min-x delta-x) (+ min-y delta-y)])]
    {:min-x min-x
     :min-y min-y
     :max-x max-x
     :max-y max-y
    ;;  :area-boundary area-boundary
    ;;  :coords        coords
     }))

(defn distance
  [[from-x from-y] [to-x to-y]]
  (+ (Math/abs (- from-x to-x)) (Math/abs (- from-y to-y))))

(comment
  (distance [1 0] [1 4]))


(defn generate-all-distance-from-coords
  [coord-map]
  (conj coord-map
        {:distances (->> (:area-boundary coord-map)
                         (reduce (fn [acc area]
                                   (conj acc {area (map (fn [coord]
                                                          [coord (distance area coord)])
                                                        (:coords coord-map))}))
                                 {}))}))

(defn remove-far-distance
  [coord-map]
  (->> (:distances coord-map)

       (reduce (fn [acc obj]
                 (let [[f s] (->> (val obj)
                                  (sort-by second)
                                  (take 2))]
                   (conj acc (if (not= (second f) (second s))
                               {(key obj) (first f)} {})))) {})))

(comment
  (->> (get-sample-data "aoc2018_6.txt")
       generate-coords-map
       generate-all-distance-from-coords
      ;;  remove-far-distance
      ;;  vals
      ;;  frequencies
      ;;  (sort-by val >)
       #_(parse-coords)))

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
