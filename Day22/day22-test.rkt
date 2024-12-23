#lang racket/base

(require rackunit
         "day22.rkt")

(check-eq? (mix 42 15) 37)

(check-eq? (prune 100000000) 16113920)

(check-eq? (next-secret 123) 15887950)