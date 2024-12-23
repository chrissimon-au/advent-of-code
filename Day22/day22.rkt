#lang racket

(require algorithms)

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

(define (split-lines input) (string-split input (string #\newline)))

(define (input->start-secrets input) (map string->number (split-lines input)))

(define (total-nth-secrets input n)
  (let ([starting-nums (input->start-secrets input)])
    (sum (map (lambda (secret) (nth-secret secret n)) starting-nums))
    ))

(define (all-secrets secret n)
  (if (eq? n 0)
      (list secret)
      (cons secret (all-secrets (next-secret secret) (- n 1)))
      )
  )

(define (most-bananas input n)
  (let* ([starting-nums (input->start-secrets input)]
         [all-secret-nums (map (lambda (secret) (all-secrets secret n)) starting-nums)]
         [_ (println all-secret-nums)]
         [deltas (map (lambda (secrets) (adjacent-map (lambda (x y) (- y x)) secrets)) all-secret-nums)]
         [_ (println deltas)]
         ) 0))

(provide
 mix
 prune
 next-secret
 nth-secret
 total-nth-secrets
 most-bananas)