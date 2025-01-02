#lang typed/racket

(module part1 typed/racket

  (require
    typed/rackunit
    racket/set
    "../common.rkt")

  (struct Position ([x : Real] [y : Real]) #:transparent)

  (define-type Grid (Setof Position))

  (define (position-from-string [input : String]) : Position
    (let*
        (
         [split-parts (string-split input ",")]
         [parts (map (lambda ([p : String]) (assert (string->number p) real?)) split-parts)]
         )
      (Position (car parts) (cadr parts))
      ))

  (define empty-grid (list->set '()))

  (define (set-toggle [set : Grid] [m : Position])
    (if (set-member? set m) (set-remove set m) (set-add set m)))

  (define (process-instruction [instruction : String] [pos : Position] [lights : Grid]) : Grid
    (case
        instruction
      [("turn on") (set-add lights pos)]
      [("turn off") (set-remove lights pos)]
      [("toggle") (set-toggle lights pos)]
      [else lights]
      )
    )

  (define (process-row [instruction : String] [row : Real] [colStart : Real] [colEnd : Real] [lights : Grid])
    (foldl
     (lambda
         ([c : Real] [l : Grid])
       (process-instruction instruction (Position c row)
                            l
                            )) lights (sequence->list (in-range colStart (+ 1 colEnd))))
    )

  (define (process-square [instruction : String] [start : Position] [end : Position] [lights : Grid])
    (let
        (
         [colStart (Position-x start)]
         [colEnd (Position-x end)]
         [rowStart (Position-y start)]
         [rowEnd (Position-y end)]
         )
      (foldl
       (lambda
           ([r : Real] [l : Grid])
         (process-row instruction r colStart colEnd l))
       lights
       (sequence->list (in-range rowStart (+ 1 rowEnd)))
       ))
    )


  (define (process-line [input : String] [lights : Grid]) : Grid
    (let*
        (
         [m (assert (regexp-match #px"(.*) ([\\d,]+) through ([\\d,]+)" input))]
         [parts : (Listof String) (map (lambda (d) (assert d string?)) (cdr m))]
         [instruction : String (car parts)]
         [start : Position (position-from-string (cadr parts))]
         [end : Position (position-from-string (caddr parts))]
         )
      (process-square instruction start end lights)
      ))

  (define (follow-instructions [input : String]) : Grid
    (foldl (lambda ([line : String] [l : Grid]) (process-line line l)) empty-grid (string-split input "\n"))
    )

  (define (number-lights-on [input : String])
    (set-count (follow-instructions input)))

  (define part1 number-lights-on)

  (module+
      test

    (define suite
      (test-suite
       "day-06-part-01"
       (check-equal? (position-from-string "5,10") (Position 5 10))
       (check-equal? (process-line "turn on 0,0 through 0,0" empty-grid) (set (Position 0 0)))
       (check-equal? (process-line "turn on 1,1 through 1,1" (process-line "turn on 0,0 through 0,0" empty-grid)) (set (Position 0 0) (Position 1 1)))
       (check-equal? (process-line "turn off 0,0 through 0,0" (process-line "turn on 0,0 through 0,0" empty-grid)) empty-grid)
       (check-equal? (process-line "toggle 0,0 through 0,0" empty-grid) (set (Position 0 0)))
       (check-equal? (process-line "toggle 0,0 through 0,0" (process-line "toggle 0,0 through 0,0" empty-grid)) empty-grid)
       (check-equal? (process-line "turn on 0,0 through 0,1" empty-grid) (set (Position 0 0) (Position 0 1)))
       (check-equal? (process-line "turn on 0,0 through 1,1" empty-grid) (set (Position 0 0) (Position 0 1) (Position 1 0) (Position 1 1)))
       (check-equal? (follow-instructions "turn on 0,0 through 3,3\ntoggle 2,2 through 4,4\nturn off 4,3 through 4,4")
                     (set
                      (Position 0 0)
                      (Position 0 1)
                      (Position 0 2)
                      (Position 0 3)
                      (Position 1 0)
                      (Position 1 1)
                      (Position 1 2)
                      (Position 1 3)
                      (Position 2 0)
                      (Position 2 1)
                      (Position 3 0)
                      (Position 3 1)
                      (Position 2 4)
                      (Position 3 4)
                      (Position 4 2)
                      ))
       ;(check-aoc part1 "sample" "1")
       ;(check-aoc part1 "test" "1")
       ))

    (provide suite)

    )
  )



(module part2 typed/racket

  (require
    typed/rackunit
    racket/set
    "../common.rkt")

  (define (part2 [_input : String]) : Number 0)

  (module+
      test

    (define suite
      (test-suite
       "day-06-part-02"
       (check-true #f)
       ;(check-aoc part2 "sample" "2")
       ;(check-aoc part2 "test" "2")
       )

      )
    (provide suite)
    )
  )


(module+ test

  (require
    (rename-in (submod ".." part1 test) [suite p1suite])
    (rename-in (submod ".." part2 test) [suite p2suite])
    typed/rackunit
    typed/rackunit/text-ui
    )

  (run-tests (test-suite
              "day06"
              p1suite
              p2suite))

  )

