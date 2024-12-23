#lang racket/base

(define (mix secret mixer)
  (bitwise-xor secret mixer))

(define (prune secret)
  0)

(provide mix prune)