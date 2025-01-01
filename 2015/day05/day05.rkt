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

(define (has-pair-twice [input : String]) : Boolean
  (let*
      (
       [windows (rolling-window 2 (string->list input))]
       [groups (group-by (lambda (p) p) windows)]
       [repeat-groups (filter (lambda ([p : (Listof Any)]) (>= (length p) 2)) groups)]
       [locations : (Listof (Listof Integer))
                  (filter-map
                   (lambda
                       ([p : (Listof Any)])
                     (indexes-of windows (car p))
                     )
                   repeat-groups)]
       [result
        (ormap
         (lambda
             ([p : (Listof Integer)])
           (or
            (> (cadr p) (+ (car p) 1))
            (> (length p) 2)
            )) locations)]
       )

    (printf "   has pair twice: ~a |~a|~a~n" result repeat-groups locations)

    result
    )
  )

(define (has-repeat-with-gap [input : String]) : Boolean
  (let*
      (
       [windows (rolling-window 3 (string->list input))]
       [result  (ormap (lambda ([p : (Listof Any)]) (equal? (car p) (caddr p))) windows)]
       )
    (printf "   has repeat with gap: ~a ~a~n" result (filter (lambda ([p : (Listof Any)]) (equal? (car p) (caddr p))) windows))
    result
    )
  )

(define (is-nice-p2 [input : String]) : Boolean
  (printf "~a:~n" input)
  (and
   (has-repeat-with-gap input)
   (has-pair-twice input)
   ))

(define (count-is-nice-p2 [input : String]) : Number
  (length (filter is-nice-p2 (string-split input "\n"))))

(define part1 count-is-nice)
(define part2 count-is-nice-p2)

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
    (check-true (has-pair-twice "xyxy"))
    (check-true (has-repeat-with-gap "abcdefeghi"))
    (check-false (has-repeat-with-gap "abcdeeghi"))
    (check-true (is-nice-p2 "qjhvhtzxzqqjkmpb"))
    (check-true (is-nice-p2 "xxyxx"))
    (check-false (is-nice-p2 "aaa"))
    (check-true (is-nice-p2 "aaaa"))
    (check-false (is-nice-p2 "uurcxstgmygtbstg"))
    (check-false (is-nice-p2 "ieodomkazucvgmuy"))
    (check-false (is-nice-p2 "galwwwgugetdohkg"))
    ;(check-aoc part1 "sample" "1")
    ;(check-aoc part2 "sample" "2")
    (check-aoc part1 "test" "1")
    (check-aoc part2 "test" "2")
    ))
  )
