#lang typed/racket

(require
  racket/set
  typed/rackunit
  typed/rackunit/text-ui
  "../common.rkt")

(struct Position ([x : Number] [y : Number]) #:transparent)

(struct SantaState ([current-location : Position] [visited-locations : (Setof Position)]))

(define (initial-santa-state) (SantaState (Position 0 0) (set (Position 0 0))) )

(define (new-position [pos : Position] [instruction : Char]) : Position
  (case instruction
    [(#\^) (Position (Position-x pos) (- (Position-y pos) 1))]
    [(#\v) (Position (Position-x pos) (+ (Position-y pos) 1))]
    [(#\<) (Position (- (Position-x pos) 1) (Position-y pos))]
    [(#\>) (Position (+ (Position-x pos) 1) (Position-y pos))]
    [else pos]
    ))

(define
  (update-santa-state [instruction : Char] [state : SantaState]) : SantaState
  (let*
      (
       [new-pos (new-position (SantaState-current-location state) instruction)]
       )
    (SantaState new-pos (set-add (SantaState-visited-locations state) new-pos))
    ))


(define
  (num-houses [input : String]) : Number
  (let* (
         [instructions (string->list input)]
         [final-state (foldl update-santa-state (initial-santa-state) instructions)]
         )
    (set-count (SantaState-visited-locations final-state))))

(define (part1 [input : String]) : Number (num-houses input))
(define (part2 [_input : String]) : Number 0)

(module+
    test

  (run-tests
   (test-suite
    "day-03"
    (check-equal? (num-houses ">") 2)
    (check-equal? (num-houses ">^") 3)
    (check-equal? (num-houses ">^v") 3)
    ;(check-aoc part1 "sample" "1")
    ;(check-aoc part2 "sample" "2")
    (check-aoc part1 "test" "1")
    ;(check-aoc part2 "test" "2")
    ))
  )