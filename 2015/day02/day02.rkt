#lang typed/racket

(require
  typed/rackunit
  racket/string
  typed/rackunit/text-ui
  "../common.rkt")




(define (area-for-present [input : String]) : Number
  (let* (
         [sides-str : (Listof String) (string-split input "x")]
         [sides : (Listof Real) (map (lambda ([s : String]) (assert (string->number s) real?)) sides-str)]
         [areas : (Listof Real) (match sides [(list l w h) (list (* l w) (* w h) (* h l))])]
         )
    (+ (apply min areas) (foldl + 0 (map (lambda ([n : Number]) : Real (assert (* n 2) real?)) areas)))
    ))

(define (area-for-presents [input : String]) : Number
  (foldl + 0 (map area-for-present (string-split input "\n"))))

(define (ribbon-length [input : String]) : Number
  (let* (
         [sides-str : (Listof String) (string-split input "x")]
         [sides : (Listof Real) (map (lambda ([s : String]) (assert (string->number s) real?)) sides-str)]
         [perimiters : (Listof Real) (match sides [(list l w h) (list (+ l l h h) (+ h h w w) (+ w w l l))])]
         [volume : Number (foldl * 1 sides)]
         )
    (+ (apply min perimiters) volume)
    ))

(define (ribbon-lengths [input : String]) : Number
  (foldl + 0 (map ribbon-length (string-split input "\n"))))

(define (part1 [input : String]) : Number (area-for-presents input))
(define (part2 [input : String]) : Number (ribbon-lengths input))

(module+
    test

  (run-tests
   (test-suite
    "day-02"
    (check-equal? (area-for-present "2x3x4") 58)
    (check-equal? (area-for-present "1x1x10") 43)
    (check-equal? (ribbon-length "2x3x4") 34)
    (check-equal? (ribbon-length "1x1x10") 14)
    ;(check-aoc part1 "sample" "1")
    ;(check-aoc part2 "sample" "2")
    (check-aoc part1 "test" "1")
    (check-aoc part2 "test" "2")
    ))
  )