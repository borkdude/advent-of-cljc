(ns aoc.y2018.d10.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    ;[aoc.y2018.d10.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(def my-test-input
  "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>")

(def my-input
  "position=<-42417,  32097> velocity=< 4, -3>\nposition=<-10502, -10533> velocity=< 1,  1>\nposition=<-53094,  32093> velocity=< 5, -3>\nposition=<-53090, -21188> velocity=< 5,  2>\nposition=< 53486,  21441> velocity=<-5, -2>\nposition=<-21142, -42496> velocity=< 2,  4>\nposition=<-42422,  32088> velocity=< 4, -3>\nposition=< 42778,  10784> velocity=<-4, -1>\nposition=< 10826,  42748> velocity=<-1, -4>\nposition=<-10449,  53401> velocity=< 1, -5>\nposition=<-42453, -21187> velocity=< 4,  2>\nposition=< 32154, -31839> velocity=<-3,  3>\nposition=<-42434, -53156> velocity=< 4,  5>\nposition=<-21149,  32097> velocity=< 2, -3>\nposition=<-10497,  53409> velocity=< 1, -5>\nposition=<-42438, -53151> velocity=< 4,  5>\nposition=< 21490,  10778> velocity=<-2, -1>\nposition=< 42831,  42753> velocity=<-4, -4>\nposition=< 53474, -42501> velocity=<-5,  4>\nposition=<-42436, -21192> velocity=< 4,  2>\nposition=< 53450, -53154> velocity=<-5,  5>\nposition=< 32118, -21185> velocity=<-3,  2>\nposition=<-21158, -10533> velocity=< 2,  1>\nposition=< 10842,  42749> velocity=<-1, -4>\nposition=< 53467,  21436> velocity=<-5, -2>\nposition=< 32130, -53160> velocity=<-3,  5>\nposition=< 32119, -53151> velocity=<-3,  5>\nposition=< 32143, -21183> velocity=<-3,  2>\nposition=<-10486,  10781> velocity=< 1, -1>\nposition=<-53098,  21433> velocity=< 5, -2>\nposition=<-10465, -42503> velocity=< 1,  4>\nposition=<-53083,  10776> velocity=< 5, -1>\nposition=< 32173, -53151> velocity=<-3,  5>\nposition=<-31782,  21436> velocity=< 3, -2>\nposition=< 32143,  10782> velocity=<-3, -1>\nposition=< 21487, -42500> velocity=<-2,  4>\nposition=<-53082,  42752> velocity=< 5, -4>\nposition=< 42775, -21183> velocity=<-4,  2>\nposition=< 53431, -53160> velocity=<-5,  5>\nposition=<-53078,  10784> velocity=< 5, -1>\nposition=<-10505,  53408> velocity=< 1, -5>\nposition=< 21507, -53151> velocity=<-2,  5>\nposition=< 42814, -53160> velocity=<-4,  5>\nposition=< 53427,  53402> velocity=<-5, -5>\nposition=<-21164,  10780> velocity=< 2, -1>\nposition=< 21461,  53404> velocity=<-2, -5>\nposition=< 32173,  21436> velocity=<-3, -2>\nposition=<-53086,  21438> velocity=< 5, -2>\nposition=< 32146, -42503> velocity=<-3,  4>\nposition=< 32159,  21439> velocity=<-3, -2>\nposition=< 53442,  42751> velocity=<-5, -4>\nposition=<-31817,  42752> velocity=< 3, -4>\nposition=< 53459, -21188> velocity=<-5,  2>\nposition=< 21458, -21191> velocity=<-2,  2>\nposition=<-21134,  53402> velocity=< 2, -5>\nposition=<-10481, -10534> velocity=< 1,  1>\nposition=<-21137, -53158> velocity=< 2,  5>\nposition=<-21110, -31839> velocity=< 2,  3>\nposition=<-21153,  21441> velocity=< 2, -2>\nposition=<-31766, -42495> velocity=< 3,  4>\nposition=< 42799, -53155> velocity=<-4,  5>\nposition=<-42449,  42752> velocity=< 4, -4>\nposition=<-53102, -10531> velocity=< 5,  1>\nposition=<-21149, -10527> velocity=< 2,  1>\nposition=<-31801,  53401> velocity=< 3, -5>\nposition=<-10462, -31842> velocity=< 1,  3>\nposition=<-31806,  42751> velocity=< 3, -4>\nposition=<-10502,  10777> velocity=< 1, -1>\nposition=<-53129,  10785> velocity=< 5, -1>\nposition=< 10803, -21186> velocity=<-1,  2>\nposition=< 42810, -31847> velocity=<-4,  3>\nposition=< 53450, -21189> velocity=<-5,  2>\nposition=< 53434,  21433> velocity=<-5, -2>\nposition=<-21126,  32094> velocity=< 2, -3>\nposition=< 53446, -53157> velocity=<-5,  5>\nposition=< 21518, -31839> velocity=<-2,  3>\nposition=<-21146, -53157> velocity=< 2,  5>\nposition=< 32135,  32089> velocity=<-3, -3>\nposition=<-53094, -21188> velocity=< 5,  2>\nposition=< 32170, -53152> velocity=<-3,  5>\nposition=<-42476,  21437> velocity=< 4, -2>\nposition=<-42446, -31840> velocity=< 4,  3>\nposition=< 53471, -42503> velocity=<-5,  4>\nposition=< 10850,  42752> velocity=<-1, -4>\nposition=<-21141, -10531> velocity=< 2,  1>\nposition=<-10508,  53404> velocity=< 1, -5>\nposition=< 42818, -21190> velocity=<-4,  2>\nposition=< 21483, -42503> velocity=<-2,  4>\nposition=<-10482, -42503> velocity=< 1,  4>\nposition=<-53089, -31847> velocity=< 5,  3>\nposition=< 32162,  32092> velocity=<-3, -3>\nposition=<-31763,  42753> velocity=< 3, -4>\nposition=<-10462, -10535> velocity=< 1,  1>\nposition=< 42831,  21433> velocity=<-4, -2>\nposition=< 42819, -42504> velocity=<-4,  4>\nposition=< 53426, -21192> velocity=<-5,  2>\nposition=< 21475, -42495> velocity=<-2,  4>\nposition=<-10462,  32092> velocity=< 1, -3>\nposition=< 42802, -10533> velocity=<-4,  1>\nposition=< 32146,  32088> velocity=<-3, -3>\nposition=< 10831, -21184> velocity=<-1,  2>\nposition=< 53466,  42752> velocity=<-5, -4>\nposition=< 21516, -42499> velocity=<-2,  4>\nposition=< 32159, -53152> velocity=<-3,  5>\nposition=<-53123,  32097> velocity=< 5, -3>\nposition=<-53126,  21434> velocity=< 5, -2>\nposition=<-31790, -21189> velocity=< 3,  2>\nposition=<-31789, -31843> velocity=< 3,  3>\nposition=<-10502, -21185> velocity=< 1,  2>\nposition=<-53124,  21441> velocity=< 5, -2>\nposition=< 53486,  10779> velocity=<-5, -1>\nposition=< 42805, -31846> velocity=<-4,  3>\nposition=<-42470,  53406> velocity=< 4, -5>\nposition=< 53469, -42495> velocity=<-5,  4>\nposition=< 53450,  42751> velocity=<-5, -4>\nposition=< 42822, -31843> velocity=<-4,  3>\nposition=< 32133,  21436> velocity=<-3, -2>\nposition=< 21487,  21437> velocity=<-2, -2>\nposition=< 32149, -31841> velocity=<-3,  3>\nposition=<-31778, -53151> velocity=< 3,  5>\nposition=< 32175, -42504> velocity=<-3,  4>\nposition=< 10834,  42750> velocity=<-1, -4>\nposition=< 10855,  10777> velocity=<-1, -1>\nposition=< 21492,  21438> velocity=<-2, -2>\nposition=<-21131,  21434> velocity=< 2, -2>\nposition=< 53455,  32091> velocity=<-5, -3>\nposition=<-31786,  53408> velocity=< 3, -5>\nposition=< 10829,  10781> velocity=<-1, -1>\nposition=< 10831, -10529> velocity=<-1,  1>\nposition=<-42460,  53405> velocity=< 4, -5>\nposition=< 10813, -31839> velocity=<-1,  3>\nposition=<-53076,  21432> velocity=< 5, -2>\nposition=<-10452, -42499> velocity=< 1,  4>\nposition=< 21487, -10534> velocity=<-2,  1>\nposition=<-53082,  10781> velocity=< 5, -1>\nposition=< 10839, -10527> velocity=<-1,  1>\nposition=< 42770,  21441> velocity=<-4, -2>\nposition=< 53466, -21185> velocity=<-5,  2>\nposition=< 10812,  42753> velocity=<-1, -4>\nposition=< 32156,  21432> velocity=<-3, -2>\nposition=< 10803,  53407> velocity=<-1, -5>\nposition=< 53430,  42747> velocity=<-5, -4>\nposition=<-53114,  32088> velocity=< 5, -3>\nposition=< 32156,  42748> velocity=<-3, -4>\nposition=< 42802, -42499> velocity=<-4,  4>\nposition=<-53110, -42500> velocity=< 5,  4>\nposition=< 21474, -31840> velocity=<-2,  3>\nposition=<-21166,  21432> velocity=< 2, -2>\nposition=<-10506,  42746> velocity=< 1, -4>\nposition=<-31819, -42499> velocity=< 3,  4>\nposition=<-31769, -10528> velocity=< 3,  1>\nposition=< 53426,  32088> velocity=<-5, -3>\nposition=<-21150, -10529> velocity=< 2,  1>\nposition=<-53086,  53408> velocity=< 5, -5>\nposition=<-31782,  32095> velocity=< 3, -3>\nposition=<-42449,  42747> velocity=< 4, -4>\nposition=<-53098, -31847> velocity=< 5,  3>\nposition=< 32162, -21185> velocity=<-3,  2>\nposition=<-53110, -42497> velocity=< 5,  4>\nposition=< 53455, -10532> velocity=<-5,  1>\nposition=< 32157, -42504> velocity=<-3,  4>\nposition=< 42802, -53155> velocity=<-4,  5>\nposition=<-53106,  42749> velocity=< 5, -4>\nposition=<-31781, -31848> velocity=< 3,  3>\nposition=<-53100,  53403> velocity=< 5, -5>\nposition=<-10508, -21187> velocity=< 1,  2>\nposition=<-42477,  53407> velocity=< 4, -5>\nposition=<-31795, -42504> velocity=< 3,  4>\nposition=<-53077, -10527> velocity=< 5,  1>\nposition=< 32133,  32097> velocity=<-3, -3>\nposition=<-53090,  53409> velocity=< 5, -5>\nposition=< 10847,  42749> velocity=<-1, -4>\nposition=< 21466, -21192> velocity=<-2,  2>\nposition=< 21476, -21192> velocity=<-2,  2>\nposition=<-10462,  21439> velocity=< 1, -2>\nposition=< 53430, -53153> velocity=<-5,  5>\nposition=< 42831, -10534> velocity=<-4,  1>\nposition=<-42435,  21432> velocity=< 4, -2>\nposition=< 32131,  53400> velocity=<-3, -5>\nposition=<-10481, -31844> velocity=< 1,  3>\nposition=<-10478,  53404> velocity=< 1, -5>\nposition=<-42438, -53160> velocity=< 4,  5>\nposition=< 32170, -53153> velocity=<-3,  5>\nposition=< 42778,  32092> velocity=<-4, -3>\nposition=<-42427,  42753> velocity=< 4, -4>\nposition=< 21476, -21183> velocity=<-2,  2>\nposition=<-21126, -21191> velocity=< 2,  2>\nposition=<-21118, -31842> velocity=< 2,  3>\nposition=<-10505, -31848> velocity=< 1,  3>\nposition=<-42421, -10536> velocity=< 4,  1>\nposition=<-31779,  21436> velocity=< 3, -2>\nposition=<-53106,  21437> velocity=< 5, -2>\nposition=< 42807, -53151> velocity=<-4,  5>\nposition=< 10810,  10778> velocity=<-1, -1>\nposition=< 32154, -10534> velocity=<-3,  1>\nposition=< 42814, -53160> velocity=<-4,  5>\nposition=< 53430,  21439> velocity=<-5, -2>\nposition=<-42462,  32097> velocity=< 4, -3>\nposition=< 21506,  10777> velocity=<-2, -1>\nposition=<-53092, -42495> velocity=< 5,  4>\nposition=<-53081, -10528> velocity=< 5,  1>\nposition=< 53471, -53153> velocity=<-5,  5>\nposition=<-53106, -21187> velocity=< 5,  2>\nposition=< 53450,  21437> velocity=<-5, -2>\nposition=< 21515, -10527> velocity=<-2,  1>\nposition=< 42791, -31846> velocity=<-4,  3>\nposition=< 21463,  21433> velocity=<-2, -2>\nposition=< 53485,  10776> velocity=<-5, -1>\nposition=< 42790, -31848> velocity=<-4,  3>\nposition=<-21122, -21192> velocity=< 2,  2>\nposition=< 21511,  53406> velocity=<-2, -5>\nposition=<-10505, -21192> velocity=< 1,  2>\nposition=<-21163,  21436> velocity=< 2, -2>\nposition=< 53426, -42496> velocity=<-5,  4>\nposition=< 32162,  10781> velocity=<-3, -1>\nposition=<-42470, -10527> velocity=< 4,  1>\nposition=<-10458,  32093> velocity=< 1, -3>\nposition=<-21105,  42745> velocity=< 2, -4>\nposition=< 53479, -31840> velocity=<-5,  3>\nposition=<-31805,  42744> velocity=< 3, -4>\nposition=< 21498,  32094> velocity=<-2, -3>\nposition=<-10486,  32094> velocity=< 1, -3>\nposition=< 21484,  10781> velocity=<-2, -1>\nposition=< 32140,  32088> velocity=<-3, -3>\nposition=<-21130,  10777> velocity=< 2, -1>\nposition=< 42794, -10533> velocity=<-4,  1>\nposition=< 21498, -21187> velocity=<-2,  2>\nposition=<-21116, -31848> velocity=< 2,  3>\nposition=< 53450, -31844> velocity=<-5,  3>\nposition=< 32142,  42745> velocity=<-3, -4>\nposition=<-31782, -53152> velocity=< 3,  5>\nposition=<-53091, -10527> velocity=< 5,  1>\nposition=< 42798,  10777> velocity=<-4, -1>\nposition=<-42422,  42744> velocity=< 4, -4>\nposition=< 10855, -10529> velocity=<-1,  1>\nposition=< 21490,  32094> velocity=<-2, -3>\nposition=<-42437, -42504> velocity=< 4,  4>\nposition=< 53483, -21192> velocity=<-5,  2>\nposition=< 53466,  53406> velocity=<-5, -5>\nposition=<-10458, -31848> velocity=< 1,  3>\nposition=< 42771,  32090> velocity=<-4, -3>\nposition=<-31780,  53404> velocity=< 3, -5>\nposition=<-53115, -10536> velocity=< 5,  1>\nposition=<-10449,  10776> velocity=< 1, -1>\nposition=<-31772,  32097> velocity=< 3, -3>\nposition=< 21515, -21186> velocity=<-2,  2>\nposition=< 42815,  42749> velocity=<-4, -4>\nposition=<-21158,  21437> velocity=< 2, -2>\nposition=< 42774, -31846> velocity=<-4,  3>\nposition=<-53105,  53405> velocity=< 5, -5>\nposition=< 21478, -10527> velocity=<-2,  1>\nposition=<-31781, -21183> velocity=< 3,  2>\nposition=< 53450,  32093> velocity=<-5, -3>\nposition=< 10810, -42499> velocity=<-1,  4>\nposition=< 32132,  10781> velocity=<-3, -1>\nposition=< 32138,  10785> velocity=<-3, -1>\nposition=< 10863, -21190> velocity=<-1,  2>\nposition=< 10859,  53409> velocity=<-1, -5>\nposition=<-53109, -42499> velocity=< 5,  4>\nposition=< 32132,  53400> velocity=<-3, -5>\nposition=< 53434,  32088> velocity=<-5, -3>\nposition=< 53479, -10530> velocity=<-5,  1>\nposition=< 53487,  21432> velocity=<-5, -2>\nposition=< 53427,  10779> velocity=<-5, -1>\nposition=<-10465,  21434> velocity=< 1, -2>\nposition=<-21149,  21432> velocity=< 2, -2>\nposition=< 10823, -42504> velocity=<-1,  4>\nposition=<-42446,  10783> velocity=< 4, -1>\nposition=<-31771,  42753> velocity=< 3, -4>\nposition=< 21501,  10780> velocity=<-2, -1>\nposition=<-53081, -31847> velocity=< 5,  3>\nposition=< 53475,  53400> velocity=<-5, -5>\nposition=<-53074, -53160> velocity=< 5,  5>\nposition=<-53126,  21441> velocity=< 5, -2>\nposition=< 10823, -21190> velocity=<-1,  2>\nposition=< 53485, -53160> velocity=<-5,  5>\nposition=<-21108,  10785> velocity=< 2, -1>\nposition=<-42457,  32097> velocity=< 4, -3>\nposition=<-10486, -53158> velocity=< 1,  5>\nposition=< 53466,  21432> velocity=<-5, -2>\nposition=< 10834,  53409> velocity=<-1, -5>\nposition=< 21493,  32090> velocity=<-2, -3>\nposition=<-31798, -21183> velocity=< 3,  2>\nposition=<-42429, -10536> velocity=< 4,  1>\nposition=<-53131,  10781> velocity=< 5, -1>\nposition=< 53459,  53404> velocity=<-5, -5>\nposition=< 53469,  53404> velocity=<-5, -5>\nposition=< 42828,  53409> velocity=<-4, -5>\nposition=<-21139, -53160> velocity=< 2,  5>\nposition=<-31769, -42499> velocity=< 3,  4>\nposition=< 42821,  10781> velocity=<-4, -1>\nposition=< 21490,  21434> velocity=<-2, -2>\nposition=< 21459, -53158> velocity=<-2,  5>\nposition=< 10842, -10533> velocity=<-1,  1>\nposition=< 42778,  32097> velocity=<-4, -3>\nposition=< 32131,  42750> velocity=<-3, -4>\nposition=< 42821, -10531> velocity=<-4,  1>\nposition=< 42775, -53159> velocity=<-4,  5>\nposition=< 42820, -42495> velocity=<-4,  4>\nposition=< 53468,  53404> velocity=<-5, -5>\nposition=<-31818,  42746> velocity=< 3, -4>\nposition=<-42422, -31840> velocity=< 4,  3>\nposition=< 21479, -21183> velocity=<-2,  2>\nposition=<-42454,  21435> velocity=< 4, -2>\nposition=< 10847, -53152> velocity=<-1,  5>\nposition=<-31813,  53409> velocity=< 3, -5>\nposition=< 10847,  21435> velocity=<-1, -2>\nposition=<-53081,  32094> velocity=< 5, -3>\nposition=<-42473,  32096> velocity=< 4, -3>\nposition=< 10807, -42503> velocity=<-1,  4>\nposition=< 10862,  32097> velocity=<-1, -3>\nposition=< 53469,  10785> velocity=<-5, -1>\nposition=< 21503, -42498> velocity=<-2,  4>\nposition=< 53430,  21435> velocity=<-5, -2>\nposition=< 53427, -31841> velocity=<-5,  3>\nposition=< 42799, -42495> velocity=<-4,  4>\nposition=<-21107, -31839> velocity=< 2,  3>\nposition=< 21511, -53151> velocity=<-2,  5>\nposition=<-42433, -10530> velocity=< 4,  1>\nposition=<-31777, -31846> velocity=< 3,  3>\nposition=<-21141, -21191> velocity=< 2,  2>\nposition=<-53100,  32094> velocity=< 5, -3>\nposition=< 21514,  42753> velocity=<-2, -4>\nposition=<-42466,  10785> velocity=< 4, -1>\nposition=<-31790,  21441> velocity=< 3, -2>\nposition=<-42433,  32095> velocity=< 4, -3>\nposition=<-42430, -42497> velocity=< 4,  4>\nposition=< 21503, -31840> velocity=<-2,  3>\nposition=< 21475, -53154> velocity=<-2,  5>\nposition=<-42449,  32095> velocity=< 4, -3>\nposition=<-53094,  10784> velocity=< 5, -1>\nposition=< 21495, -21192> velocity=<-2,  2>\nposition=< 53434, -21191> velocity=<-5,  2>\nposition=< 32143, -21189> velocity=<-3,  2>\nposition=<-21140, -53155> velocity=< 2,  5>\nposition=< 21495,  42744> velocity=<-2, -4>\nposition=< 21502,  32097> velocity=<-2, -3>\nposition=<-53094, -42495> velocity=< 5,  4>\nposition=<-42474,  53406> velocity=< 4, -5>\nposition=<-21141, -10535> velocity=< 2,  1>\nposition=< 42788, -31843> velocity=<-4,  3>\nposition=<-21139, -21192> velocity=< 2,  2>\nposition=<-10493,  10782> velocity=< 1, -1>")

(defn parse [input]
  (->> input
       str/split-lines
       (map #(->> %
                  (re-seq #"-*\d+")
                  (map u/parse-int)))
       (map (fn [[x y dx dy]]
              {:x x
               :y y
               :dx dx
               :dy dy}))))

(defn step [[x y] [dx dy]]
  [(+ x dx)
   (+ y dy)])

(defn min-max-by [pred coll]
  (when coll
    (let [value-predvalue-pairs (map (fn [x] [x (pred x)]) coll)]
      (map first (reduce (fn [[min-pair max-pair] new-pair]
                           [(min-key second min-pair new-pair)
                            (max-key second max-pair new-pair)])
                         [(first value-predvalue-pairs) (first value-predvalue-pairs)]
                         (rest value-predvalue-pairs))))))

(defn y-height [stars]
  (let [[y-min y-max] (min-max-by identity (map second stars))]
    (- y-max y-min)))

(defn stars-to-string [stars]
  (let [[x-min x-max] (map first (min-max-by first stars))
        [y-min y-max] (map second (min-max-by second stars))
        stars-set (into #{} stars)]
    (->> (for [y (range y-min (inc y-max))]
           (apply str (map (fn [x]
                             (if (stars-set [x y]) \* \space))
                           (range x-min (inc x-max)))))
         (str/join \newline))))

(defn print-stars [stars]
  (let [[x-min x-max] (map first (min-max-by first stars))
        [y-min y-max] (map second (min-max-by second stars))
        stars-set (into #{} stars)]
    (doseq [line (for [y (range y-min (inc y-max))]
                   (apply str (map (fn [x]
                                     (if (stars-set [x y]) \* \space))
                                   (range x-min (inc x-max)))))]
      (println line))))

(defn left-most-x-neighbor [all-points point]
  (let [x-vals (into #{} (map first all-points))]
    (loop [x (first point)]
      (if (x-vals (dec x))
        (recur (dec x))
        x))))

(def star-letters
  {"*    *\n*    *\n *  * \n *  * \n  **  \n  **  \n *  * \n *  * \n*    *\n*    *" \X
   "*     \n*     \n*     \n*     \n*     \n*     \n*     \n*     \n*     \n******" \L
   "******\n     *\n     *\n    * \n   *  \n  *   \n *    \n*     \n*     \n******" \Z
   "  **  \n *  * \n*    *\n*    *\n*    *\n******\n*    *\n*    *\n*    *\n*    *" \A
   "*    *\n*   * \n*  *  \n* *   \n**    \n**    \n* *   \n*  *  \n*   * \n*    *" \K
   "***** \n*    *\n*    *\n*    *\n***** \n*    *\n*    *\n*    *\n*    *\n***** " \B
   " **** \n*    *\n*     \n*     \n*     \n*  ***\n*    *\n*    *\n*   **\n *** *" \G})

(defn solve-1 []
  (let [parsed-input (parse my-input)
        velocities (map #(map % [:dx :dy]) parsed-input)]
    (->> (loop [stars (map #(map % [:x :y]) parsed-input)
                height (y-height stars)
                time 0]
           (let [new-stars (map step stars velocities)
                 new-height (y-height new-stars)]
             (if (> new-height height)
               stars
               (recur new-stars new-height (inc time)))))
         (#(group-by (partial left-most-x-neighbor %) %))
         sort
         (map second)
         (map stars-to-string)
         (map star-letters)
         (apply str))))

(defn solve-2 []
  (let [parsed-input (parse my-input)
        velocities (map #(map % [:dx :dy]) parsed-input)]
    (loop [stars (map #(map % [:x :y]) parsed-input)
           height (y-height stars)
           time 0]
      (let [new-stars (map step stars velocities)
            new-height (y-height new-stars)]
        (if (> new-height height)
          time
          (recur new-stars new-height (inc time)))))))

;(deftest part-1
;  (is (= (str answer-1)
;         (str (solve-1)))))
;
;(deftest part-2
;  (is (= (str answer-2)
;         (str (solve-2)))))
