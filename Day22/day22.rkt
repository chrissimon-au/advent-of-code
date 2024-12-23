#lang racket/base

(define (mix secret mixer)
  (bitwise-xor secret mixer))

(provide mix)