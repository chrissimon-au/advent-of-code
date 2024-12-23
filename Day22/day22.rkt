#lang racket/base

(define (mix secret mixer)
  (bitwise-xor secret mixer))

(define (prune secret)
  ;(bitwise-and secret (- 16777215 1))
  (modulo secret 16777216)
  )


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

(define (nth-secret secret n)
  (if (eq? n 0) secret (nth-secret (next-secret secret) (- n 1))))

(provide
 mix
 prune
 next-secret
 nth-secret
 step1
 step2
 step3)