(ns aoc2020_4
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]))

;; # Day 4

;; [https://adventofcode.com/2020/day/4](https://adventofcode.com/2020/day/4)

;; ## 파트 1
;; 여권이 유효한지 판단하려고 한다. 여권에는 다음과 같은 필드가 있음.
;; - byr (Birth Year)
;; - iyr (Issue Year)
;; - eyr (Expiration Year)
;; - hgt (Height)
;; - hcl (Hair Color)
;; - ecl (Eye Color)
;; - pid (Passport ID)
;; - cid (Country ID)

;; 파트 1에서는 여권의 모든 필드가 존재하는지의 여부를 검사한다. 주어진 입력에서 '유효한' 여권의 숫자를 반환하여라.

;; ```
;; ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
;; byr:1937 iyr:2017 cid:147 hgt:183cm

;; iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
;; hcl:#cfa07d byr:1929

;; hcl:#ae17e1 iyr:2013
;; eyr:2024
;; ecl:brn pid:760753108 byr:1931
;; hgt:179cm

;; hcl:#cfa07d eyr:2025 pid:166559648
;; iyr:2011 ecl:brn hgt:59in
;; ```

;; - 첫번째는 유효한 여권이다. 8개의 필드가 전부 존재한다.
;; - 두번째는 유효하지 않다. hgt가 없기 때문.
;; - 세번째는 cid가 없지만, ** cid는 없어도 되는 ** 것으로 간주한다. 그래서 유효하다.
;; - 네번째는 cid와 byr이 없다. byr은 반드시 있어야하는 필드이므로 유효하지 않다.



(s/def :v/passport
  (s/keys :req-un [:passport/byr
                   :passport/iyr
                   :passport/eyr
                   :passport/hgt
                   :passport/hcl
                   :passport/ecl
                   :passport/pid]
          :opt-un [:passport/cid]))

(s/def :animal/common (s/keys :req [:animal/kind :animal/says]))

(defn is-valid-keys-passport?
  [passport-map]
  (s/valid? :v/passport passport-map))

(comment
  (into {} [`(1 2) `(2 3) `(3 4)]))

(comment
  (keyword (str "a" "/" "b")))

(defn get-sample-data [path]
  (let [separate-text (-> path
                          (io/resource)
                          (slurp)
                          (string/split #"\n\n"))]
    (map (fn [passport] (->> (string/replace passport "\n" " "))) separate-text)))

(defn string-passport-to-passport-map
  "string을 파싱하여 map형태로 변환
   input: iyr:2013 hcl:#ceb3a1 hgt:151cm eyr:2030 byr:1943 ecl:grn
   output: {:iyr 2013, :hcl #ceb3a1, :hgt 151cm, :eyr 2030, :byr 1943, :ecl grn}"
  [s-passport]
  (->> (#(partition 2 (clojure.string/split s-passport #"[: ]")))
       (map (fn [[key value]] [(keyword key) value]))
       (into {})))

(comment
  (->> (get-sample-data "aoc2020_4.txt")
       (map string-passport-to-passport-map)
       (filter is-valid-keys-passport?)
       count))

;; ## 파트 2
;; 파트1에서는 필드의 유무만을 검사했다면, 파트2에서는 구체적인 범위가 주어진다.
;; - byr (Birth Year) - 4 자리 숫자; 최소 1920 & 최대 2002.
;; - iyr (Issue Year) - 4 자리 숫자; 최소 2010 & 최대 2020.
;; - eyr (Expiration Year) - 4 자리 숫자; 최소 2020 & 최대 2030.
;; - hgt (Height) - 마지막에 cm 혹은 in이 오는 숫자:
;; - cm의 경우, 숫자는 최소 150 & 최대 193.
;; - in의 경우, 숫자는 최소 59 & 최대 76.
;; - hcl (Hair Color) - #뒤에 오는 정확히 6개의 캐릭터 0-9 혹은 a-f.
;; - ecl (Eye Color) - 정확히 amb blu brn gry grn hzl oth 중 하나.
;; - pid (Passport ID) - 처음 0을 포함하는 9자리 숫자.
;; - cid (Country ID) - 없어도 됨.

;; 아래는 예시들이다.
;; ```
;; byr valid:   2002
;; byr invalid: 2003

;; hgt valid:   60in
;; hgt valid:   190cm
;; hgt invalid: 190in
;; hgt invalid: 190

;; hcl valid:   #123abc
;; hcl invalid: #123abz
;; hcl invalid: 123abc

;; ecl valid:   brn
;; ecl invalid: wat

;; pid valid:   000000001
;; pid invalid: 0123456789
;; ```
;; 모든 필드의 기준에 맞는 여권의 수를 반환하여라.

(defn convert-str-to-height
  "str로 되어있는 값을 height unit으로 구분
  input: 190cm
  output: [190 cm]"
  [s]
  (if-let [[_ height unit]
           (re-matches #"([0-9]+)(in|cm)" s)]
    [(Integer/parseInt height) unit]
    nil))

(defn is-valid-hgt?
  "cm in 에 따라서 범위를 다르게 검사"
  [[height unit]]
  (case unit
    "cm" (s/int-in-range? 150 (+ 193 1) height)
    "in" (s/int-in-range? 59 (+ 76 1) height)
    false))

(defn is-valid-pid?
  "숫자 9자리인지 검사"
  [pid]
  (when pid
    (re-matches #"[0-9]{9}" pid)))

(defn is-valid-hcl?
  "color hex코드인지 확인"
  [heir-color]
  (when heir-color
    (re-matches #"#[0-9|a-f]{6}" heir-color)))

(s/def :p/passport
  (s/keys :req-un [:p/byr
                   :p/iyr
                   :p/eyr
                   :p/hgt
                   :p/hcl
                   :p/ecl
                   :p/pid]
          :opt-un [:p/cid]))

(s/def :p/byr #(s/int-in-range? 1920 (+ 2002 1) %))
(s/def :p/iyr #(s/int-in-range? 2010 (+ 2020 1) %))
(s/def :p/eyr #(s/int-in-range? 2020 (+ 2030 1) %))
(s/def :p/hgt #(is-valid-hgt? %))
(s/def :p/pid #(is-valid-pid? %))
(s/def :p/hcl #(is-valid-hcl? %))
(s/def :p/ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn is-valid-passport?
  [passport]
  (s/valid? :p/passport passport))

(comment
  (is-valid-passport? {:ecl "brn"
                       :pid "160033328"
                       :eyr 2030
                       :hcl "#ffffff"
                       :byr 2002
                       :iyr 2020
                       :cid 147
                       :hgt [190 "cm"]}))

(defn parse-passport
  "str로 돼 있는 값들을 검증하기 전에 타입을 맞춥니다."
  [passport]
  (->> passport
       (map (fn [[k v]] [k v]
              (case k
                (:byr :iyr :eyr) [k (Integer/parseInt v)]
                :hgt [k (convert-str-to-height v)]
                [k v])))
       (into {})
       (merge passport)))

(comment
  (->> (get-sample-data "aoc2020_4.txt")
       (map string-passport-to-passport-map)
       (map parse-passport)
       (filter is-valid-passport?)
       count))