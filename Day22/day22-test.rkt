#lang racket/base

(require rackunit
         "day22.rkt")

(check-eq? (mix 42 15) 37)

(check-eq? (prune 100000000) 16113920)

(check-eq? (next-secret 123) 15887950)
(check-eq? (next-secret 15887950) 16495136)
(check-eq? (next-secret 16495136) 527345)
(check-eq? (next-secret 527345) 704524)
(check-eq? (next-secret 704524) 1553684)
(check-eq? (next-secret 1553684) 12683156)
(check-eq? (next-secret 12683156) 11100544)
(check-eq? (next-secret 11100544) 12249484)
(check-eq? (next-secret 12249484) 7753432)
(check-eq? (next-secret 7753432) 5908254)
