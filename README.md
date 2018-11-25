# advent-of-spec

Advent of Code puzzles checked with [speculative](https://github.com/slipset/speculative)

## Rationale

The purpose of this repo is to check
[speculative](https://github.com/slipset/speculative) specs for usage that was
not accounted for yet. As a bonus undefined usage of core functions
may be detected.

This library is also usable as a corpus of Clojure programs, like [coal-mine](https://github.com/mfikes/coal-mine).

## Inclusion

PRs welcome.

## Run

Run all tests:

    script/test

Run one test:

    script/test-one # random
    script/test-one nth 3 # runs day 3
    
Run without instrumentation:

    script/test-unstrumented
