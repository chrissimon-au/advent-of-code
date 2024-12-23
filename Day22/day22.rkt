#lang racket/base

(define (mix secret mixer)
  (bitwise-xor secret mixer))

(define (prune secret)
  (bitwise-and secret (- 16777215 1)))

(provide mix prune)