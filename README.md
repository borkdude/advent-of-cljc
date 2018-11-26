# advent-of-spec

Advent of Code puzzles checked with [speculative](https://github.com/slipset/speculative)

## Rationale

The purpose of this repo is to check
[speculative](https://github.com/slipset/speculative) specs for usage that was
not accounted for yet. As a bonus undefined usage of core functions
may be detected.

This library is also usable as a corpus of Clojure programs, like [coal-mine](https://github.com/mfikes/coal-mine).

## Run

Run all tests:

    script/test

Run one test:

    script/test-one aos.y2017.d01.borkdude
    
Run without instrumentation:

    UNSTRUMENT=true script/test
    UNSTRUMENT=true script/test-one aos.y2017.d01.borkdude

## Inclusion

PRs welcome. Use the following template:

``` clojure
(ns aos.y20nn.dnn.you
  (:require
   [aos.y20nn.dnn.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [deftest is testing]]))

(deftest part-1
  (is true))

(deftest part-2
  (is true))
```

Replace `20nn` and `dnn` with the correct year and day, and replace `you` with
your Github/BitBucket/etc. username.  Make sure the tests for your solution pass
with the `test-one` script.
