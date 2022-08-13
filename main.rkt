#lang racket/base

(require qi)
(require "cofunction.rkt")

(provide id
         bool->1+1
         n< 1< 2< 3< 4< 5< 6< 7< 8< 9<
         ◁ ▷
         ≂ ≂?
         procedure-coarity
         procedure-result-coarity
         ->N:
         >>> <<<
         values->list
         list->values
         (all-from-out qi)
         (for-space qi
                    ~>
                    ==+ >-
                    >>> <<<
                    <>))

(define id (procedure-reduce-arity values 1))
(define bool->1+1 (->N: #t #f))


(define-qi-syntax-rule (~> flo ...)
  (esc (make-composed (☯ flo) ...)))


(define-qi-syntax-rule (>- flo ...)
  (esc (make-copairing (☯ flo) ...)))

(define-qi-syntax-rule (==+ flo ...)
  (esc (make-coproducting (☯ flo) ...)))


(define-qi-syntax-rule (<<< args ...)
  (esc (<<< args ...)))

(define-qi-syntax-rule (>>> args ...)
  (esc (>>> args ...)))


(define-qi-syntax-rule (<> flo)
  (esc (coamp (☯ flo))))
