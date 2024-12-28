#lang typed/racket

(require
  typed/rackunit
  typed/rackunit/text-ui)

(define
  (string-count [str : String] [ch : Char]) : Number
  (length (filter (lambda (c) (eq? c ch)) (string->list str)))
  )

(define
  (final-floor [instructions : String]) : Number
  (- (string-count instructions #\() (string-count instructions #\)))
  )

(module+ test

  (define (file-contents [file-name : Path-String]) : String
    (string-trim (port->string (open-input-file file-name) #:close? #t)))


  (define day01
    (test-suite
     "day-01"
     (check-equal? (string-count "((" #\() 2)
     (check-equal? (final-floor "(())") 0)
     (check-equal? (final-floor "()()") 0)
     (check-equal? (final-floor "(((") 3)
     (check-equal? (final-floor (file-contents "day01.test.data")) (string->number (file-contents "day01.test-answer.data")))
     ))
  (run-tests day01)
  )




