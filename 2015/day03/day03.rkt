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
  (follow-instructions [instructions : (Listof Char)])
  (foldl update-santa-state (initial-santa-state) instructions))

(define
  (num-houses [input : String]) : Number
  (let* (
         [instructions (string->list input)]
         [final-state (follow-instructions instructions)]
         )
    (set-count (SantaState-visited-locations final-state))))


(: alternating-elements (All (A) (-> (Listof A) (Listof A))))
(define (alternating-elements lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list (car lst)))
        (else (cons (car lst) (alternating-elements (cddr lst))))))

(define
  (num-houses-with-robo-santa [input : String]) : Number
  (let* (
         [instructions (string->list input)]
         [santa-instructions (alternating-elements instructions)]
         [robosanta-instructions (alternating-elements (cdr instructions))]
         [santa-final-state (follow-instructions santa-instructions)]
         [robosanta-final-state (follow-instructions robosanta-instructions)]
         [all-locations
          (set-union
           (SantaState-visited-locations santa-final-state)
           (SantaState-visited-locations robosanta-final-state))]
         )
    (set-count all-locations)))

(define part1 num-houses)
(define part2 num-houses-with-robo-santa)

(module+
    test

  (run-tests
   (test-suite
    "day-03"
    (check-equal? (num-houses ">") 2)
    (check-equal? (num-houses ">^") 3)
    (check-equal? (num-houses ">^v") 3)
    (check-equal? (num-houses-with-robo-santa "^v") 3)
    (check-equal? (num-houses-with-robo-santa "^v^v^v^v^v") 11)
    ;(check-aoc part1 "sample" "1")
    ;(check-aoc part2 "sample" "2")
    (check-aoc part1 "test" "1")
    (check-aoc part2 "test" "2")
    ))
  )