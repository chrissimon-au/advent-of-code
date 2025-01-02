#lang typed/racket

(require
  typed/rackunit
  typed/rackunit/text-ui
  "../common.rkt")




(define (part1 [_input : String]) : Number 0)
(define (part2 [_input : String]) : Number 0)

(module+
    test

  (run-tests
   (test-suite
    "day-DD"
    ;(check-aoc part1 "sample" "1")
    ;(check-aoc part1 "test" "1")
    ;(check-aoc part2 "sample" "2")
    ;(check-aoc part2 "test" "2")
    ))
  )