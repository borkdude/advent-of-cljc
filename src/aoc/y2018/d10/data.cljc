(ns aoc.y2018.d10.data)

(def input "position=<-50429,  40580> velocity=< 5, -4>
position=< 30528, -40359> velocity=<-3,  4>
position=< 20386, -40351> velocity=<-2,  4>
position=< -9924,  30462> velocity=< 1, -3>
position=<-30203, -50470> velocity=< 3,  5>
position=< 50746, -40351> velocity=<-5,  4>
position=< 50778, -20120> velocity=<-5,  2>
position=<-20046,  10229> velocity=< 2, -1>
position=< 40645,  30467> velocity=<-4, -3>
position=<-50419, -10005> velocity=< 5,  1>
position=<-20089,  -9999> velocity=< 2,  1>
position=<-50386, -10003> velocity=< 5,  1>
position=< 40613, -20117> velocity=<-4,  2>
position=<-40272,  50699> velocity=< 4, -5>
position=< 20406, -50467> velocity=<-2,  5>
position=< -9972, -10000> velocity=< 1,  1>
position=< -9921,  10229> velocity=< 1, -1>
position=< 40635, -30240> velocity=<-4,  3>
position=<-50389, -30237> velocity=< 5,  3>
position=< 20390,  10231> velocity=<-2, -1>
position=<-40325,  50694> velocity=< 4, -5>
position=< 20382,  20350> velocity=<-2, -2>
position=< 50749, -10007> velocity=<-5,  1>
position=<-20046,  40584> velocity=< 2, -4>
position=< 40633, -20120> velocity=<-4,  2>
position=< 20416, -10005> velocity=<-2,  1>
position=< 30520,  40577> velocity=<-3, -4>
position=< 20377, -10004> velocity=<-2,  1>
position=< 40608,  20348> velocity=<-4, -2>
position=<-40309, -30237> velocity=< 4,  3>
position=< 10305, -20123> velocity=<-1,  2>
position=< 40669,  30464> velocity=<-4, -3>
position=<-30150,  50700> velocity=< 3, -5>
position=<-20090, -40359> velocity=< 2,  4>
position=< 30496,  20352> velocity=<-3, -2>
position=< 20387,  10235> velocity=<-2, -1>
position=<-50445,  40581> velocity=< 5, -4>
position=< 20398, -20119> velocity=<-2,  2>
position=< 20374,  10232> velocity=<-2, -1>
position=< 20414, -50472> velocity=<-2,  5>
position=< -9916,  -9999> velocity=< 1,  1>
position=< 50765,  40585> velocity=<-5, -4>
position=<-50426,  50694> velocity=< 5, -5>
position=< -9920,  50699> velocity=< 1, -5>
position=<-50408,  30464> velocity=< 5, -3>
position=< 50781, -20123> velocity=<-5,  2>
position=<-30211,  30462> velocity=< 3, -3>
position=< 40617, -20116> velocity=<-4,  2>
position=< 50765,  10227> velocity=<-5, -1>
position=<-20066,  40585> velocity=< 2, -4>
position=<-30170,  40582> velocity=< 3, -4>
position=< 10290,  50698> velocity=<-1, -5>
position=<-40318,  10235> velocity=< 4, -1>
position=< 50785,  10227> velocity=<-5, -1>
position=< 30526,  10230> velocity=<-3, -1>
position=< -9977, -20120> velocity=< 1,  2>
position=<-40327, -20121> velocity=< 4,  2>
position=< 10299, -30236> velocity=<-1,  3>
position=< 40632, -20117> velocity=<-4,  2>
position=< -9958,  10230> velocity=< 1, -1>
position=<-50420, -50471> velocity=< 5,  5>
position=< 10281,  50699> velocity=<-1, -5>
position=< 50749,  50700> velocity=<-5, -5>
position=< 50786,  20351> velocity=<-5, -2>
position=< 50776,  20348> velocity=<-5, -2>
position=< 50733, -30238> velocity=<-5,  3>
position=< -9932, -30242> velocity=< 1,  3>
position=< 40612,  30466> velocity=<-4, -3>
position=< 30496, -20122> velocity=<-3,  2>
position=<-30174,  30461> velocity=< 3, -3>
position=< 30534, -50474> velocity=<-3,  5>
position=< 40661,  -9999> velocity=<-4,  1>
position=< 30552,  50696> velocity=<-3, -5>
position=< 50775,  30464> velocity=<-5, -3>
position=<-20057,  20345> velocity=< 2, -2>
position=< 20410,  50698> velocity=<-2, -5>
position=< 10297, -50474> velocity=<-1,  5>
position=<-50429, -20118> velocity=< 5,  2>
position=<-40327, -20121> velocity=< 4,  2>
position=<-30194, -20121> velocity=< 3,  2>
position=< -9956,  40580> velocity=< 1, -4>
position=< 20427, -10008> velocity=<-2,  1>
position=< 10257, -20122> velocity=<-1,  2>
position=<-20075, -40355> velocity=< 2,  4>
position=<-20041,  10234> velocity=< 2, -1>
position=<-20033,  30468> velocity=< 2, -3>
position=<-50389,  30464> velocity=< 5, -3>
position=< 50773,  -9999> velocity=<-5,  1>
position=<-30179, -50474> velocity=< 3,  5>
position=< 40659,  30465> velocity=<-4, -3>
position=< 50781, -20123> velocity=<-5,  2>
position=< 40632, -10001> velocity=<-4,  1>
position=<-30194, -30238> velocity=< 3,  3>
position=<-20073, -20117> velocity=< 2,  2>
position=< 50730,  10228> velocity=<-5, -1>
position=< 10313,  40579> velocity=<-1, -4>
position=<-50421, -10003> velocity=< 5,  1>
position=< -9924,  40581> velocity=< 1, -4>
position=<-30151, -30237> velocity=< 3,  3>
position=< -9965,  20343> velocity=< 1, -2>
position=< 50759,  50698> velocity=<-5, -5>
position=<-20036, -30237> velocity=< 2,  3>
position=<-20070, -20122> velocity=< 2,  2>
position=< 40641, -50472> velocity=<-4,  5>
position=<-40293,  50698> velocity=< 4, -5>
position=< 50736, -30233> velocity=<-5,  3>
position=<-50389,  20351> velocity=< 5, -2>
position=< 20374, -30234> velocity=<-2,  3>
position=<-20043, -20119> velocity=< 2,  2>
position=<-50445, -10005> velocity=< 5,  1>
position=< -9961, -20116> velocity=< 1,  2>
position=<-40328,  40578> velocity=< 4, -4>
position=<-50405,  10226> velocity=< 5, -1>
position=< 10305, -40359> velocity=<-1,  4>
position=< -9929, -40358> velocity=< 1,  4>
position=< 30499, -50470> velocity=<-3,  5>
position=< 50741, -30234> velocity=<-5,  3>
position=< -9965,  20348> velocity=< 1, -2>
position=<-20062, -30238> velocity=< 2,  3>
position=<-20066, -30234> velocity=< 2,  3>
position=< 50773,  10235> velocity=<-5, -1>
position=< 20430, -40353> velocity=<-2,  4>
position=<-40311, -40359> velocity=< 4,  4>
position=< 50757, -30236> velocity=<-5,  3>
position=<-30198,  20350> velocity=< 3, -2>
position=< 40632, -10006> velocity=<-4,  1>
position=<-50444, -40355> velocity=< 5,  4>
position=< 20422,  10232> velocity=<-2, -1>
position=<-50393,  50701> velocity=< 5, -5>
position=<-50429, -40350> velocity=< 5,  4>
position=<-20046, -30238> velocity=< 2,  3>
position=<-50397, -30233> velocity=< 5,  3>
position=< 40618, -10008> velocity=<-4,  1>
position=<-40271, -10007> velocity=< 4,  1>
position=<-20069,  10230> velocity=< 2, -1>
position=<-30154,  40578> velocity=< 3, -4>
position=<-20057,  50696> velocity=< 2, -5>
position=< 40634, -20119> velocity=<-4,  2>
position=< 30523, -10007> velocity=<-3,  1>
position=< 10305,  50694> velocity=<-1, -5>
position=< 30547, -10003> velocity=<-3,  1>
position=< 10257, -50468> velocity=<-1,  5>
position=< 40628,  10230> velocity=<-4, -1>
position=<-20078, -30233> velocity=< 2,  3>
position=<-40312, -50471> velocity=< 4,  5>
position=< 30500,  20343> velocity=<-3, -2>
position=< 10273,  40585> velocity=<-1, -4>
position=< 10314, -30237> velocity=<-1,  3>
position=< 20414,  40582> velocity=<-2, -4>
position=< 30531,  50701> velocity=<-3, -5>
position=<-30170, -20120> velocity=< 3,  2>
position=<-50413, -20120> velocity=< 5,  2>
position=< -9945, -40359> velocity=< 1,  4>
position=<-40275,  30465> velocity=< 4, -3>
position=<-40312,  40583> velocity=< 4, -4>
position=<-30179,  40580> velocity=< 3, -4>
position=< -9974,  30465> velocity=< 1, -3>
position=< 10284,  10228> velocity=<-1, -1>
position=< 20374,  50694> velocity=<-2, -5>
position=< -9937,  50701> velocity=< 1, -5>
position=<-50441,  50701> velocity=< 5, -5>
position=< 10270, -50468> velocity=<-1,  5>
position=< 50773,  50697> velocity=<-5, -5>
position=< 40619, -40359> velocity=<-4,  4>
position=<-40315,  40586> velocity=< 4, -4>
position=<-30179,  10232> velocity=< 3, -1>
position=<-20049,  30460> velocity=< 2, -3>
position=< 10289, -50473> velocity=<-1,  5>
position=<-20033, -20119> velocity=< 2,  2>
position=< 50766, -40355> velocity=<-5,  4>
position=<-30191,  50701> velocity=< 3, -5>
position=< 10273,  30463> velocity=<-1, -3>
position=< 50778,  -9999> velocity=<-5,  1>
position=< 50730,  50695> velocity=<-5, -5>
position=< -9929, -50473> velocity=< 1,  5>
position=< 40644, -20121> velocity=<-4,  2>
position=< 50725, -40352> velocity=<-5,  4>
position=<-50388,  40582> velocity=< 5, -4>
position=< 50741,  20345> velocity=<-5, -2>
position=<-20090,  40581> velocity=< 2, -4>
position=<-30191, -10008> velocity=< 3,  1>
position=< 10273,  20348> velocity=<-1, -2>
position=< 10268,  20343> velocity=<-1, -2>
position=< 30533,  40580> velocity=<-3, -4>
position=<-20041, -40358> velocity=< 2,  4>
position=< 30531,  50703> velocity=<-3, -5>
position=< 50757, -30234> velocity=<-5,  3>
position=<-30187,  -9999> velocity=< 3,  1>
position=<-50442,  40582> velocity=< 5, -4>
position=< 50749, -30242> velocity=<-5,  3>
position=<-30158, -20121> velocity=< 3,  2>
position=<-50386, -20125> velocity=< 5,  2>
position=< 40637,  10235> velocity=<-4, -1>
position=<-30203, -20117> velocity=< 3,  2>
position=<-50392, -20118> velocity=< 5,  2>
position=< 30493,  20347> velocity=<-3, -2>
position=<-30163,  40582> velocity=< 3, -4>
position=< 30499,  40580> velocity=<-3, -4>
position=<-50392, -50474> velocity=< 5,  5>
position=<-20085,  10235> velocity=< 2, -1>
position=< 10313,  20346> velocity=<-1, -2>
position=< 40636,  50695> velocity=<-4, -5>
position=<-50384, -30235> velocity=< 5,  3>
position=< -9956,  -9999> velocity=< 1,  1>
position=< 30547,  50703> velocity=<-3, -5>
position=< -9957, -50472> velocity=< 1,  5>
position=<-30174,  10232> velocity=< 3, -1>
position=< 30544,  40586> velocity=<-3, -4>
position=< 30552,  40582> velocity=<-3, -4>
position=< 40611,  40577> velocity=<-4, -4>
position=< 30549, -50476> velocity=<-3,  5>
position=< -9945, -50473> velocity=< 1,  5>
position=<-40296, -40352> velocity=< 4,  4>
position=<-30163, -20117> velocity=< 3,  2>
position=<-50405, -40357> velocity=< 5,  4>
position=< 50786, -30236> velocity=<-5,  3>
position=< 50757, -20124> velocity=<-5,  2>
position=< 20392,  10230> velocity=<-2, -1>
position=< 30531,  20348> velocity=<-3, -2>
position=< 50735,  10235> velocity=<-5, -1>
position=<-30198, -30233> velocity=< 3,  3>
position=<-50397, -50469> velocity=< 5,  5>
position=<-50441,  30467> velocity=< 5, -3>
position=<-30163, -10000> velocity=< 3,  1>
position=< 50773, -40355> velocity=<-5,  4>
position=<-20086, -30234> velocity=< 2,  3>
position=< 10276,  40577> velocity=<-1, -4>
position=< 40661,  50700> velocity=<-4, -5>
position=< 50728,  50699> velocity=<-5, -5>
position=< 10313, -40353> velocity=<-1,  4>
position=<-50445,  50697> velocity=< 5, -5>
position=<-20050,  10227> velocity=< 2, -1>
position=< 10315, -40359> velocity=<-1,  4>
position=< 20379,  40579> velocity=<-2, -4>
position=< 30507, -40359> velocity=<-3,  4>
position=<-50444, -50476> velocity=< 5,  5>
position=< 40637, -40350> velocity=<-4,  4>
position=< 20427, -10005> velocity=<-2,  1>
position=< 10318,  10233> velocity=<-1, -1>
position=<-30190,  30463> velocity=< 3, -3>
position=< 10300,  20345> velocity=<-1, -2>
position=<-50408,  40578> velocity=< 5, -4>
position=< 10297, -10002> velocity=<-1,  1>
position=<-20060,  40581> velocity=< 2, -4>
position=< 40640,  20352> velocity=<-4, -2>
position=<-50437,  10231> velocity=< 5, -1>
position=<-50429, -20119> velocity=< 5,  2>
position=<-50396, -50474> velocity=< 5,  5>
position=<-20078, -40358> velocity=< 2,  4>
position=< 20376, -30242> velocity=<-2,  3>
position=< 30528,  50695> velocity=<-3, -5>
position=<-20043, -30236> velocity=< 2,  3>
position=< 40632,  10226> velocity=<-4, -1>
position=<-50413,  20344> velocity=< 5, -2>
position=< -9940, -40356> velocity=< 1,  4>
position=< 10308,  30465> velocity=<-1, -3>
position=<-30171, -20118> velocity=< 3,  2>
position=<-20066,  30461> velocity=< 2, -3>
position=<-40291, -40351> velocity=< 4,  4>
position=<-30191,  50700> velocity=< 3, -5>
position=<-20054, -10007> velocity=< 2,  1>
position=< 10270,  30461> velocity=<-1, -3>
position=< -9940, -30234> velocity=< 1,  3>
position=<-30150,  50703> velocity=< 3, -5>
position=< -9965, -50476> velocity=< 1,  5>
position=< 40664, -10002> velocity=<-4,  1>
position=<-40324,  10230> velocity=< 4, -1>
position=< -9924, -40354> velocity=< 1,  4>
position=< 20416,  20349> velocity=<-2, -2>
position=<-50397, -50471> velocity=< 5,  5>
position=< 40610, -30242> velocity=<-4,  3>
position=< -9964, -30236> velocity=< 1,  3>
position=<-20094,  30464> velocity=< 2, -3>
position=< 40660, -10000> velocity=<-4,  1>
position=<-40311,  50698> velocity=< 4, -5>
position=<-20091, -50472> velocity=< 2,  5>
position=<-30154, -20124> velocity=< 3,  2>
position=<-50389,  10229> velocity=< 5, -1>
position=<-20094,  20344> velocity=< 2, -2>
position=<-20074, -30235> velocity=< 2,  3>
position=<-50433,  30468> velocity=< 5, -3>
position=<-20054, -30238> velocity=< 2,  3>
position=< -9945, -10006> velocity=< 1,  1>
position=< -9945,  20347> velocity=< 1, -2>
position=<-30193,  30460> velocity=< 3, -3>
position=< 40632,  10227> velocity=<-4, -1>
position=<-40315,  20344> velocity=< 4, -2>
position=<-40303,  50698> velocity=< 4, -5>
position=< 30526,  40581> velocity=<-3, -4>
position=<-40300,  10234> velocity=< 4, -1>
position=< 30507,  50700> velocity=<-3, -5>
position=<-20083, -50471> velocity=< 2,  5>
position=< 50733,  20344> velocity=<-5, -2>
position=< -9969, -10006> velocity=< 1,  1>
position=<-30174,  30469> velocity=< 3, -3>
position=< 40660,  50702> velocity=<-4, -5>
position=<-50393, -50468> velocity=< 5,  5>
position=<-20054,  30462> velocity=< 2, -3>
position=<-50404, -10004> velocity=< 5,  1>
position=<-50440,  20351> velocity=< 5, -2>
position=<-20042, -20118> velocity=< 2,  2>
position=< 10289, -40350> velocity=<-1,  4>
position=<-30162,  50695> velocity=< 3, -5>
position=<-30171,  20349> velocity=< 3, -2>
position=<-50413, -20125> velocity=< 5,  2>
position=<-50424, -50467> velocity=< 5,  5>
position=<-40315,  20348> velocity=< 4, -2>
position=<-50392, -50470> velocity=< 5,  5>
position=< 10281, -50473> velocity=<-1,  5>
position=< 20395, -30240> velocity=<-2,  3>
position=< 30499, -40356> velocity=<-3,  4>
position=< 30531,  50697> velocity=<-3, -5>
position=< 50766,  10230> velocity=<-5, -1>
position=<-40291, -40355> velocity=< 4,  4>
position=<-40291,  50699> velocity=< 4, -5>
position=< 20418,  30461> velocity=<-2, -3>
position=<-50400, -50467> velocity=< 5,  5>
position=<-30202,  10226> velocity=< 3, -1>
position=< 30540,  30462> velocity=<-3, -3>
position=< 20376, -10008> velocity=<-2,  1>
position=< -9953, -50473> velocity=< 1,  5>
position=< 30512,  30468> velocity=<-3, -3>
position=<-40328, -10001> velocity=< 4,  1>
position=< -9933, -40351> velocity=< 1,  4>
position=< 40645,  30460> velocity=<-4, -3>
position=<-40316,  50699> velocity=< 4, -5>
position=< -9965, -50476> velocity=< 1,  5>
position=<-20046, -50468> velocity=< 2,  5>
position=< -9921,  40584> velocity=< 1, -4>
position=<-40275,  10232> velocity=< 4, -1>
position=< -9916,  30467> velocity=< 1, -3>
position=<-50445,  20349> velocity=< 5, -2>
position=<-30168, -20118> velocity=< 3,  2>
position=< 50773, -40354> velocity=<-5,  4>
position=<-50397,  40584> velocity=< 5, -4>
position=< 20392, -10008> velocity=<-2,  1>
position=<-40272, -20116> velocity=< 4,  2>
position=<-20078,  30468> velocity=< 2, -3>
position=< 10299,  20346> velocity=<-1, -2>
position=<-40328, -50469> velocity=< 4,  5>
position=< 20395,  50695> velocity=<-2, -5>
position=<-20086, -30236> velocity=< 2,  3>
position=<-30169,  10232> velocity=< 3, -1>
position=<-50396,  10227> velocity=< 5, -1>
position=<-20081,  10232> velocity=< 2, -1>
position=< 20430,  50701> velocity=<-2, -5>
position=< 50775, -10005> velocity=<-5,  1>
position=< 40648, -20125> velocity=<-4,  2>
position=<-30191,  10230> velocity=< 3, -1>
position=<-20062, -10002> velocity=< 2,  1>
position=< 50745,  40577> velocity=<-5, -4>
position=<-30179, -30237> velocity=< 3,  3>
position=< 30544, -20125> velocity=<-3,  2>
position=<-30187,  20347> velocity=< 3, -2>
position=< 50773,  20343> velocity=<-5, -2>
position=<-40267,  40580> velocity=< 4, -4>
position=< 20406, -10000> velocity=<-2,  1>
position=<-40291,  30465> velocity=< 4, -3>
position=<-20084,  50694> velocity=< 2, -5>
position=< 50725, -20124> velocity=<-5,  2>
position=<-20078,  10230> velocity=< 2, -1>
position=< 30528, -20119> velocity=<-3,  2>
position=< 10273,  10227> velocity=<-1, -1>
position=< -9964,  40584> velocity=< 1, -4>
position=<-50445,  10235> velocity=< 5, -1>
position=< 50729, -30236> velocity=<-5,  3>
position=< -9916,  30468> velocity=< 1, -3>
position=<-30167, -20124> velocity=< 3,  2>
position=< 10270, -20117> velocity=<-1,  2>
position=<-50405,  30465> velocity=< 5, -3>
position=<-30184, -50469> velocity=< 3,  5>
position=<-40312,  40584> velocity=< 4, -4>
position=<-20065,  10226> velocity=< 2, -1>
position=< 50762,  -9999> velocity=<-5,  1>
position=< 40648, -50475> velocity=<-4,  5>
position=< 20401,  30467> velocity=<-2, -3>
position=<-20081, -10000> velocity=< 2,  1>
position=<-30190,  10229> velocity=< 3, -1>
position=<-50389,  20350> velocity=< 5, -2>
position=< 30520,  10235> velocity=<-3, -1>
position=< -9951, -40356> velocity=< 1,  4>
position=< 40659, -30236> velocity=<-4,  3>
position=< 50762,  30467> velocity=<-5, -3>
position=<-50402, -50469> velocity=< 5,  5>
position=< 40624, -30235> velocity=<-4,  3>
position=< 40629, -30233> velocity=<-4,  3>
position=< -9918,  20348> velocity=< 1, -2>
position=<-30203, -30239> velocity=< 3,  3>
position=<-50387,  20348> velocity=< 5, -2>
position=< 30528, -20125> velocity=<-3,  2>
position=< 20384, -20125> velocity=<-2,  2>")

(def answer-1 "RGRKHKNA")

(def answer-2 10117)
