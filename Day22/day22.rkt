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

(define ((nth-secret n) secret)
  (if (eq? n 0)
      secret
      ((nth-secret (- n 1)) (next-secret secret)))
  )

(define (split-lines input) (string-split input (string #\newline)))

(define (input->start-secrets input) (map string->number (split-lines input)))

(define (total-nth-secrets n input)
  (let ([starting-nums (input->start-secrets input)])
    (sum (map (nth-secret n) starting-nums))
    ))

(define ((all-secrets n) secret)
  (if (eq? n 0)
      (list secret)
      (cons secret ((all-secrets (- n 1)) (next-secret secret)))
      )
  )

(define ((mapped f) l) (map f l))

(define (get-cost secret) (remainder secret 10))

(define (get-deltas secrets) (adjacent-map (lambda (x y) (cons (- y x) y)) secrets))

(define ((slide k) lst) (sliding lst k))

(define (to-window-with-result chunk)
  (let ([deltas (map car chunk)]
        [costs (map cdr chunk)]
        )
    (cons deltas (last costs))
    ))

(define (group-outcomes-by-chunk lst)
  (group-by (lambda (chunk) (car chunk)) lst equal?)
  )

(define (get-first-instruction-instance lst)
  (map car lst)
  )

(define (get-bananas-per-instructions list)
  (sum (map cdr list))
  )

(define (most-bananas n input)
  (let* ([all-secret-nums ((compose (mapped (all-secrets n)) input->start-secrets) input)]
         [all-instructionsets ((compose (mapped get-first-instruction-instance) (mapped group-outcomes-by-chunk) (mapped (mapped to-window-with-result)) (mapped (slide 4)) (mapped get-deltas) (mapped (mapped get-cost))) all-secret-nums)]
         [all-instructions (foldl append '() all-instructionsets)]
         )
    (argmax (lambda (x) x) ((compose (mapped get-bananas-per-instructions) group-outcomes-by-chunk) all-instructions))
    ))

(provide
 mix
 prune
 next-secret
 nth-secret
 total-nth-secrets
 most-bananas)