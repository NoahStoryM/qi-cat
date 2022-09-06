#lang racket/base

(require
 racket/function
 syntax/parse/define)

(provide (all-defined-out))

(define (->boolean v) (and v #t))
(define (true?  arg) (eq? #t arg))
(define (false? arg) (eq? #f arg))
(define (thunk->list thk) (call-with-values thk list))
(define (list->thunk lst) (thunk (list->values lst)))
(define (list->values lst) (apply values lst))
(define-syntax-parse-rule (values->list body:expr ...+)
  (call-with-values (thunk body ...) list))
