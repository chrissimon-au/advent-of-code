#lang typed/racket

(require
  typed/rackunit
  typed/rackunit/text-ui
  "../common.rkt")

(define
  (string-count [str : String] [ch : Char]) : Number
  (length (filter (lambda (c) (eq? c ch)) (string->list str)))
  )

(define
  (final-floor [instructions : String]) : Number
  (- (string-count instructions #\() (string-count instructions #\)))
  )

(define
  (apply-instruction [current-floor : Number] [instruction : Char]) : Number
  (if (eq? instruction #\()
      (+ current-floor 1)
      (- current-floor 1))
  )

(define
  (position-of-floor-calc [instructions : (Listof Char)] [target-floor : Number] [current-floor : Number] [instruction-counter : Number]) : Number
  (if (eq? current-floor target-floor)
      instruction-counter
      (position-of-floor-calc
       (cdr instructions)
       target-floor
       (apply-instruction current-floor (car instructions))
       (+ instruction-counter 1)
       ))
  )

(define
  (position-of-floor [instructions : String] [target-floor : Number]) : Number
  (position-of-floor-calc (string->list instructions) target-floor 0 0)
  )

(define (part1 [input : String]) : Number (final-floor input))
(define (part2 [input : String]) : Number (position-of-floor input -1))

(module+ test
  (run-tests
   (test-suite
    "day-01"
    (check-equal? (string-count "((" #\() 2)
    (check-equal? (final-floor "(())") 0)
    (check-equal? (final-floor "()()") 0)
    (check-equal? (final-floor "(((") 3)
    (check-aoc part1 "test" "1")
    (check-aoc part2 "test" "2")
    ))
  )




