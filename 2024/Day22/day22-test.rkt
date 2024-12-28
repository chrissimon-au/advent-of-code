#lang racket

(require rackunit"day22.rkt")

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

(check-eq? ((nth-secret 2000) 1) 8685429)
(check-eq? ((nth-secret 2000) 10) 4700978)
(check-eq? ((nth-secret 2000) 100) 15273692)
(check-eq? ((nth-secret 2000) 2024) 8667524)

(check-eq? (total-nth-secrets 2000 #<<EOF
1
10
100
2024
EOF
                              ) 37327623 "AoC Sample Data")

(define (file-contents file-name)
  (string-trim (port->string (open-input-file file-name) #:close? #t)))

(check-eq? (total-nth-secrets 2000 (file-contents "testdata.txt") )
           (string->number (file-contents "testdata.answer.txt")) "AoC Test Data")


(check-eq? (most-bananas 2000 #<<EOF
1
2
3
2024
EOF
                         ) 23 "AoC Sample Data Part 2")

(check-eq? (most-bananas 2000 (file-contents "testdata.txt") )
           (string->number (file-contents "testdata.answer2.txt")) "AoC Test Data")