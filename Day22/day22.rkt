#lang racket/base

(define (mix secret mixer)
  (bitwise-xor secret mixer))

(define (prune secret)
  (bitwise-and secret (- 16777215 1)))


(define (step1 secret)
  (prune
   (mix secret
        (arithmetic-shift secret 6)
        )
   ))

(define (step2 secret)
  (prune
   (mix secret
        (arithmetic-shift secret -5)
        )
   ))

(define (step3 secret)
  (prune
   (mix secret
        (arithmetic-shift secret 11)
        )
   ))

(define (next-secret secret)
  (step3 (step2 (step1 secret))))

(provide mix prune next-secret)