(ns aoc2018_7
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))

;; # Day 7

;; [https://adventofcode.com/2018/day/7](https://adventofcode.com/2018/day/7)

;; ## 파트 1

;; 스케줄이 주어질 때, 일이 처리되는 순서를 반환하시오.
;; 알파벳 캐릭터 하나로 대표되는 일(work)이 주어지고, 각 일을 처리하기 위해서 선행되어야 하는 일들이 스케줄 형식으로 주어짐.
;; ```
;; Step C must be finished before step A can begin.
;; Step C must be finished before step F can begin.
;; Step A must be finished before step B can begin.
;; Step A must be finished before step D can begin.
;; Step B must be finished before step E can begin.
;; Step D must be finished before step E can begin.
;; Step F must be finished before step E can begin.
;; ```
;; 위와 같은 입력의 경우 아래 형태의 그래프가 만들어짐.


;; ```
;;   -->A--->B--
;;  /    \      \
;; C      -->D----->E
;;  \           /
;;   ---->F-----
;; ```

;; 순서는 아래와 같음.
;; - 처음엔 C만 가능함. C에서 시작. 만약 다른 시작점이 존재한다면 알파벳 순서로 진행.
;; - C 다음으로 A와 F가 가능한데, A가 알파벳 우선순위가 높으므로 A로 감.
;; - A 를 완료하면 B, D, F가 가능한데, 역시 알파벳 우선순위가 높은 B가 수행됨.
;; - 남은 D와 F중에서 D가 수행됨
;; - F가 수행됨
;; - E가 수행됨 (E는 B, D, F 모두가 선행되어야 수행될 수 있음)

;; 결과: `CABDFE`


(defn parse-steps
  "정규식으로 Step을 추출
  input: Step B must be finished before step E can begin.
  output: [B E]"
  [str]
  (let [[first second] (->> (re-matches #"Step ([A-Z]+) must be finished before step ([A-Z]+) can begin." str)
                            (take-last 2))]
    [first second]))

(comment (parse-steps "Step B must be finished before step E can begin."))


(defn get-sample-data [path]
  (->> path
       (io/resource)
       (slurp)
       (string/split-lines)))

(defn get-requirement-step-map
  "특정 스탭을 처리하기 위해서 필수로 처리해야하는 스탭들을 정리합니다.
  input: ([C A] [C F] [A B] [A D] [B E] [D E] [F E])
  output: {
            E (B D F)
            C []
            F (C)
            B (A)
            A (C)
            D (A)
          }"
  [input]
  (let [default-step-map (->> input
                              flatten
                              set
                              (map #(vector % []))
                              (into {}))]

    (reduce (fn [acc [step next-step]]
              (assoc acc next-step (sort (conj (get acc next-step []) step))))
            default-step-map input)))

(defn generate-trace-step
  "들어온 리스트를 제외한 가능한 작업을 나열하고 우선순위가 높은 일감을 처리 하는 작업을 반복"
  [step-map trace]
  (let [pre-step-map   (:pre-step-map step-map)
        all-steps      (:all-chraters step-map)
        all-left-steps (->> (set/difference all-steps (set trace)) sort)
        can-do-steps   (->> all-left-steps
                            (filter (fn [c]
                                      (empty? (set/difference
                                               (set (get pre-step-map c))
                                               (set trace))))))
        current-step   (first can-do-steps)
        trace          (if (or (nil? current-step)
                               (contains? (set trace) current-step))
                         trace
                         (conj trace current-step))]
    (if (empty? can-do-steps) trace (recur step-map trace))))

(comment
  "part 1"
  (let [input        (->> (get-sample-data "aoc2018_7.txt")
                          (map parse-steps))
        pre-step-map (get-requirement-step-map input)
        step-map     {:pre-step-map pre-step-map
                      :all-chraters (->> input flatten set)}]

    (->> (generate-trace-step step-map [])
         string/join)))

(comment
  (->> (range)
       (take 10)))

;; ## 파트 2

;; 파트 1에서는 일을 워커(worker)가 하나였지만, 파트 2는 5명. 즉, 동시에 5개의 일을 처리할 수 있게 됨.
;; 그리고 각각의 일 (A\~Z)은 처리하는데 (60+1\~60+26)의 시간이 걸림. B는 62초, D는 64초, etc.

;; 이 때, 주어진 모든 일을 처리하는데 걸리는 시간을 구하시오.

;; 예)

;; CABDFE

;; 아래는 파트 1의 예시에서 워커가 2명이 된 케이스이다.
;; ```
;; Second   Worker 1   Worker 2   Done
;;    0        C          .        
;;    1        C          .        
;;    2        C          .        
;;    3        A          F       C
;;    4        B          F       CA
;;    5        B          F       CA
;;    6        D          F       CAB
;;    7        D          F       CAB
;;    8        D          F       CAB
;;    9        D          .       CABF
;;   10        E          .       CABFD
;;   11        E          .       CABFD
;;   12        E          .       CABFD
;;   14        E          .       CABFDE
;; ```
;; 15초가 걸리므로 답은 15

(defn get-processing-second
  "처리 시간을 반환합니다. 
  input: A
  output: 61
  "
  [step]
  (- (int (.charAt step 0)) 4))

(defn create-workers
  "pk, work, finished-time 값을 가지고 있는 워커를 반환합니다."
  [worker-count]
  (->> (range 1 (+ worker-count 1))
       (map (fn [number] {:pk            number
                          :work          nil
                          :finished-time 0}))))

(comment
  (get-processing-second "C")
  (create-workers 5))

(defn assign-works-to-iddle-workers
  "일이 없는 워커에게 일을 할당하고 할당된 일감과 워커들을 반환합니다.
  input: [[{:work A :finished-time 100} {:work nil :finished-time 0}] [B D F] 123]
  output: [[{:work A :finished-time 100} {:work B :finished-time 125}] [B]]"
  [workers works current-time]

  (loop [workers        workers
         left-works     works
         assigned-works []]
    (if (or (empty? left-works)
            (empty? (filter (fn [worker]
                              (nil? (:work worker))) workers)))
      [workers assigned-works]

      (let [iddle-workers      (->> workers
                                    (filter (fn [worker]
                                              (nil? (:work worker)))))
            first-work         (first left-works)
            left-works         (rest left-works)
            assigned-worker    (assoc (first iddle-workers) :work first-work
                                      :finished-time (+ (get-processing-second first-work)
                                                        current-time))
            assigned-works     (conj assigned-works first-work)
            left-iddle-workers (rest iddle-workers)
            working-workers    (filter (fn [worker]
                                         (some? (:work worker)))
                                       workers)
            workers            (concat (list assigned-worker) left-iddle-workers working-workers)]

        (recur workers left-works assigned-works)))))


(comment (assign-works-to-iddle-workers [{:id            1
                                          :work          "A"
                                          :finished-time 3100}
                                         {:id            2
                                          :work          nil
                                          :finished-time nil}
                                         {:id            3
                                          :work          nil
                                          :finished-time 0}]
                                        ["Z" "D" "V"] 70))

(defn clean-up-finished-works
  "완료된 일감을 반환하고 워커들을 iddle 상태로 변경 합니다.
  input: [{:work A :finished-time: 123}] 123
  output: [{:work nil :finished-time: 0}] [A]"
  [workers current-time]

  (let [finished-works (->> workers
                            (filter (fn [worker]
                                      (not (nil? (:work worker)))))
                            (reduce (fn [acc worker]
                                      (if (= current-time (:finished-time worker))
                                        (conj acc (:work worker)) acc))
                                    []))
        workers        (map (fn [worker]
                              (if (= current-time (:finished-time worker))
                                (assoc worker :work nil :finished-time 0) worker)) workers)]

    [workers finished-works]))

(comment (clean-up-finished-works [{:id            1
                                    :work          "A"
                                    :finished-time 71}
                                   {:id            2
                                    :work          "B"
                                    :finished-time 73}
                                   {:id            3
                                    :work          "C"
                                    :finished-time 74}]
                                  71))

(comment
  (type (concat #{"A" "C" "B"} #{"A" "B" "C" "D"})))

(defn get-spending-time-by-workers
  "할당된 워커 할 수 있는 일감을 처리합니다. 모든 작업이 마무리되면 작업시간을 반환합니다."
  [[current-time pre-work-map all-works workers ongoing-works finished-works]]

  (let [[workers just-finished-works] (clean-up-finished-works workers current-time)
        finished-works                (set/union finished-works just-finished-works)
        can-do-works                  (->> all-works
                                           (filter (fn [c]
                                                     (empty? (set/difference
                                                              (set (get pre-work-map c))
                                                              (set finished-works))))))
        can-do-works                  (->> (set/difference (set can-do-works) ongoing-works) sort)
        [workers assigned-works]      (assign-works-to-iddle-workers workers can-do-works current-time)
        ongoing-works                 (concat assigned-works ongoing-works)]

    [(inc current-time) pre-work-map all-works workers ongoing-works finished-works]))

(comment
  "day7 part2"
  (let [input        (->> (get-sample-data "aoc2018_7.txt")
                          (map parse-steps))
        pre-step-map (get-requirement-step-map input)
        all-chraters (->> input flatten set)]

    (->> [0 pre-step-map all-chraters (create-workers 5) [] []]
         (iterate get-spending-time-by-workers)
         (drop-while (fn [[_ _ all-works _ _ finished-works]] (not= (set all-works) (set finished-works))))
         ffirst)))