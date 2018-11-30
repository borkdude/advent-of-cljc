# advent-of-spec
[![CircleCI](https://circleci.com/gh/borkdude/advent-of-spec/tree/master.svg?style=svg)](https://circleci.com/gh/borkdude/advent-of-spec/tree/master)

Cross platform Clojure Advent of Code solutions.

## Why the name?

Because it sounds better than Advent of CLJC. And because the solutions can be optionally checked with [speculative](https://github.com/slipset/speculative).

## Contribute

What's in it for you?
* Your Advent of Code solutions will be checked against the same input as others. This diminishes the possibility that your solution only works for your specific input.
* You will be encouraged to write portable Clojure code: a solution that runs on the JVM via Clojure and on Node via ClojureScript.
* [Speculative](https://github.com/slipset/speculative) may help you find undefined or incorrect usage of Clojure core functions.
* As instrumentation is performance constraining, you are encouraged to find a faster solution.

What's in it for the Clojure community?
* You are helping advance the [speculative](https://github.com/slipset/speculative) project, a collection of core specs.
* You are helping to build a large Clojure corpus for various purposes (see the Rationale for [coal-mine](https://github.com/mfikes/coal-mine)).

PRs welcome. Make a new solution file with the `new` script:

    script/new 2017 1 borkdude

and fill in the details.

Make sure the tests for your solution pass with the `test-one` script.

## Run

Run all tests:

    script/test

Run one test:

    script/test-one aos.y2017.d01.borkdude
    
Run with instrumentation:

    INSTRUMENT=true script/test
    INSTRUMENT=true script/test-one aos.y2017.d01.borkdude

Skip Clojure or ClojureSCript:

    SKIP_CLJ=true script/test
    SKIP_CLJS=true script/test
