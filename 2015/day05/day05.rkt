#lang typed/racket

(require
  typed/rackunit
  typed/rackunit/text-ui
  "../common.rkt")

; From https://stackoverflow.com/a/40520792/6720449
(define (rolling-window [n : Integer] [xs : (Listof Any)]) : (Listof (Listof Any))
  (let*
      (
       [v (list->vector xs)]
       [m (vector-length v)]
       [l : Real (assert (- m n -1) real?)]
       [l : Integer (assert (truncate (max 0 l)) exact-nonnegative-integer?)]
       )
    (for/list ([i l])
      (vector->list (vector-copy v i (+ i n))))))

(define
  (has-double-letter [input : String]) : Boolean
  (ormap
   (lambda ([p : (Listof Any)]) : Boolean (equal? (car p) (cadr p)))
   (rolling-window 2 (string->list input)))
  )

(define
  (has-no-forbidden-substrings [input : String]) : Boolean
  (not (
        or
        (string-contains? input "ab")
        (string-contains? input "cd")
        (string-contains? input "pq")
        (string-contains? input "xy")
        ))
  )

(define
  (is-vowel? [ c : Char ]) : Boolean
  (or
   (equal? c #\a)
   (equal? c #\e)
   (equal? c #\i)
   (equal? c #\o)
   (equal? c #\u)))

(define
  (has-vowels [n : Real] [input : String]) : Boolean
  (>= (length (filter is-vowel? (string->list input))) n)
  )

(define
  (is-nice [input : String]) : Boolean
  (and
   (has-double-letter input)
   (has-no-forbidden-substrings input)
   (has-vowels 3 input)
   ))

(define
  (count-is-nice [input : String]) : Number
  (length (filter is-nice (string-split input "\n"))))


(define part1 count-is-nice)
(define (part2 [_input : String]) : Number 0)

(module+
    test

  (run-tests
   (test-suite
    "day-05"
    (check-true (is-nice "aaa"))
    (check-false (is-nice "jchzalrnumimnmhp"))
    (check-false (is-nice "haegwjzuvuyypxyu"))
    (check-false (is-nice "dvszwmarrgswjxmb"))
    (check-true (is-nice "ugknbfddgicrmopn"))
    ;(check-aoc part1 "sample" "1")
    ;(check-aoc part2 "sample" "2")
    (check-aoc part1 "test" "1")
    ;(check-aoc part2 "test" "2")
    ))
  )