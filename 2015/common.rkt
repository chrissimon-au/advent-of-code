#lang typed/racket

(require
  typed/rackunit)

(define (file-contents [file-name : Path-String]) : String
  (string-trim (port->string (open-input-file file-name) #:close? #t)))

(define (file-contents-number [file-name : Path-String]) : (U Complex False)
  (string->number (file-contents file-name)))

(define (check-aoc [fn : (String -> Number)] [base : String] [suffix : String]) : Any
  (check-equal? (fn (file-contents (string-append base ".data"))) (file-contents-number (string-append base "-answer" suffix ".data"))))


(provide
 file-contents
 file-contents-number
 check-aoc)