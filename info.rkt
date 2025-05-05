#lang info

(define license 'Unlicense)
(define collection "qi")
(define version "0.0")

(define pkg-desc "Qi based on Category Theory")

(define deps '("base" "variant"))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
