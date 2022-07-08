(ns aoc2018_4
  (:require [clojure.string :as string]
            [clojure.instant :as instant]
            [clj-time.core :as t]))
;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

(defn get-sample-data [path]
  (->> (slurp path)
       (string/split-lines)
       sort))

(comment
  (defn convert-to-time
    "date와 time 문자를 받아 Date 형태로 변경
    input ('2022-01-01' '00:00:00)
    output: Date
    "
    [d t]
    (->
     (str d "T" t)
     instant/read-instant-date)))


(comment

  "1518-09-14 00:54")


(defn parse-raw-line-to-map
  "가공되지 않은 데이터를 특수문자 제거 후 Date, log 로 구분 
     input: '[1518-05-24 23:57] Guard #523 begins shift'
     output: [Date ('Guard' '523' 'begins' 'shift')]
     "
  [raw-s]
  (let [[d t & log]      (-> raw-s
                             (string/replace #"[\[\]#]" "")
                             (string/split #" "))
        [year month day] (string/split d #"-")
        [hour min]       (string/split t #":")
        action           (first log)
        joined-log       (string/join #" " log)
        guard-id         (when-let [guard-id (re-matches #"[0-9]+" (nth log 1))]
                           (Integer/parseInt guard-id))]

    {:year     (Integer/parseInt year)
     :month    (Integer/parseInt month)
     :day      (Integer/parseInt day)
     :hour     hour
     :min      (Integer/parseInt min)
     :guard-id guard-id
     :action   action
     :log      joined-log}))


(defn insert-guard-id-to-action
  [logs]
  (->> logs
       (reduce (fn [acc m]
                 (if-let [last-discoverd-guard-id (:guard-id m)]
                   (assoc acc :guard-id last-discoverd-guard-id)
                   (update acc :actions conj (assoc m :guard-id (:guard-id acc)))))
               {:actions  []
                :guard-id nil})
       (:actions)))



(defn group-by-guard-id
  [logs]
  (group-by :guard-id logs))


(comment
  (partition 2 2 '(1 2 3 4 5)))
;; (defn compute-sp)


(comment
  (range 10 (+ 20 1)))

(comment
  (defn compute-sleep-time
    [actions]
    (->> (partition 2 2 actions)
         (map (fn  [[falls wakes]]
                (let [guard-id    (:guard-id wakes)
                      date        [(:year wakes) (:month wakes) (:day wakes)]
                      wake-time   (:min wakes)
                      fall-time   (:min falls)
                      sleep-time  (- wake-time fall-time)
                      all-minutes (range fall-time (+ wake-time 0))]
                  {:date        date
                   :guard-id    guard-id
                   :sleep-time  sleep-time
                   :wake-time   wake-time
                   :fall-time   fall-time
                   :all-minutes all-minutes}))))))


(comment
  (defn compute-stats
    [computed-map]
    (->> (group-by :guard-id computed-map)
         seq
         (map (fn [[id m]]
                {:guard-id           id
                 :sum-sleep-time     (->> (map (fn [obj] (:sleep-time obj)) m)
                                          (reduce +))
                 :all-minutes        (flatten (map (fn [obj] (:all-minutes obj)) m))
                 :often-sleep-minute (->> (map (fn [obj] (:all-minutes obj)) m)
                                          flatten
                                          frequencies
                                          (sort-by second #(compare %2 %1))
                                          (map (fn [line] (zipmap [:key :value] line))))})))))

(comment
  (defn compute-stats2
    [computed-map]
    (->> (group-by :guard-id computed-map)
         seq
         (map (fn [[id m]]
                {:guard-id           id
                ;;  :day                (map (fn [obj] (:day obj)) m)
                ;;  :sleep-days         (->> (group-by :date m))

                 :sum-sleep-time     (->> (map (fn [obj] (:sleep-time obj)) m)
                                          (reduce +))
                 :all-minutes        (flatten (map (fn [obj] (:all-minutes obj)) m))
                 :often-sleep-minute (->> (map (fn [obj] (:all-minutes obj)) m)
                                          flatten
                                          frequencies)})))))


(comment

  (fn [id m]
    (->> (map (fn [obj] (:sleep-time obj)) m))))

(comment
  "day4 part1"
  (->> (get-sample-data "./resources/aoc2018_4.txt")
       (map parse-raw-line-to-map)
       insert-guard-id-to-action
       compute-sleep-time
       compute-stats
       (sort-by :sum-sleep-time #(compare %2 %1))
       first))


(comment
  "day4 part1"
  (let [most-minute (->> (get-sample-data "./resources/aoc2018_4.txt")
                         (map parse-raw-line-to-map)
                         insert-guard-id-to-action
                         compute-sleep-time
                         compute-stats
                         (map :all-minutes)
                         flatten
                         frequencies
                         (sort-by val #(compare %2 %1)))]
    most-minute
    (->> (get-sample-data "./resources/aoc2018_4.txt")
         (map parse-raw-line-to-map)
         insert-guard-id-to-action
         compute-sleep-time
         (group-by :guard-id)
         seq
         (map (fn [[k v]]
                [k (->> (flatten (map :all-minutes v))
                        frequencies
                        (sort-by val >)
                        first)])))))

;; filter (fn [[k v]] (= (first v) most-minute))

