# Advent of CLJC
[![CircleCI](https://circleci.com/gh/borkdude/advent-of-cljc/tree/master.svg?style=svg)](https://circleci.com/gh/borkdude/advent-of-cljc/tree/master)

Cross platform Clojure Advent of Code solutions.

## Contribute

What's in it for you?
* Most of all, you will be encouraged to write portable Clojure code: a solution that runs on the JVM via Clojure and on Node via ClojureScript.
* Your Advent of Code solutions will be checked against the same input as others. This diminishes the possibility that your solution only works for your specific input.
* The performance of your solution can be compared with others via CircleCI (go to [CircleCI](https://circleci.com/gh/borkdude/advent-of-cljc) and open `script/test`)

What's in it for the Clojure community?
* You are helping advance the [speculative](https://github.com/slipset/speculative) project, a collection of core specs.
* You are helping to build a large Clojure corpus for various purposes (see the Rationale for [coal-mine](https://github.com/mfikes/coal-mine)).

PRs welcome. Make a new solution file with the `new` script:

    script/new 2017 1 username

where `username` is your Github or Bitbucket username. Then fill in the solution in the file. If the input and answers are still empty you will have to provide it in `data.cljc`.

This repo will not accept multiple input and answers (see [this issue](https://github.com/borkdude/advent-of-cljc/issues/6) for details).

Make sure the tests for your solution pass with the `test-one` script.

Please do not run calculations outside the tests. Memoized functions are permitted. Top-level lazy sequences are fine as long as they are not realized outside the tests.

## Run

Run all tests:

    script/test

Run one test:

    script/test-one 2017 1 username
    
Run with instrumentation:

    INSTRUMENT=true script/test
    INSTRUMENT=true script/test-one aoc.y2017.d01.username

Skip Clojure or ClojureScript:

    SKIP_CLJ=true script/test
    SKIP_CLJS=true script/test
