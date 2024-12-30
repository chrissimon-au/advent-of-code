#lang typed/racket

(require
  typed/rackunit
  typed/rackunit/text-ui
  typed/file/md5
  "../common.rkt")


(define (md5-starts-with [ input : String ] [ expected-prefix : String ] ) : Boolean
  (let* (
         [md5-string (md5 input)]
         [prefix-length (string-length expected-prefix)]
         [prefix (take (bytes->list md5-string) prefix-length)]
         [prefix-bytes (list->bytes prefix)]
         [expected-prefix-bytes (string->bytes/utf-8 expected-prefix)]
         )
    (equal? prefix-bytes expected-prefix-bytes )))

(: find-suffix-for-prefix  (->* (String String) (Number) Number))
(define (find-suffix-for-prefix [ input : String ] [ expected-prefix : String ] [ suffix : Number 0 ]) : Number
  (if
   (md5-starts-with (string-append input (number->string suffix)) expected-prefix)
   suffix
   (find-suffix-for-prefix input expected-prefix (+ suffix 1))
   )
  )

(define (part1 [input : String]) : Number (find-suffix-for-prefix input "00000"))
(define (part2 [input : String]) : Number (find-suffix-for-prefix input "000000"))

(module+
    test

  (run-tests
   (test-suite
    "day-04"
    (check-true (md5-starts-with "abcdef609043" "00000"))
    (check-false (md5-starts-with "pqrstuv1048969" "00000"))
    (check-eq? (find-suffix-for-prefix "abcdef" "00000") 609043)
    (check-eq? (find-suffix-for-prefix "pqrstuv" "00000") 1048970)
    ;(check-aoc part1 "sample" "1")
    ;(check-aoc part2 "sample" "2")
    (check-aoc part1 "test" "1")
    (check-aoc part2 "test" "2")
    ))
  )