#lang racket/base

(require qi)
(require "cofunction.rkt")

(provide id
         bool->1+1
         n< 0< 1< 2< 3< 4< 5< 6< 7< 8< 9<
         f0->f
         fanin
         ◁ ▷
         ≂ ≂?
         procedure-coarity
         procedure-result-coarity
         >>> <<<
         values->list
         list->values
         (all-from-out qi)
         (for-space qi
                    ~> ~>>
                    ==+ >-
                    <>
                    =<
                    let/cc let/ec))


(define-qi-syntax-rule (~>  flo ...)
  (esc (make-composed (☯ flo) ...)))

(define-qi-syntax-rule (~>> flo ...)
  (esc (make-composed (☯ flo) ...)))


(define-qi-syntax-rule (>-  flo ...)
  (esc (make-copairing (☯ flo) ...)))

(define-qi-syntax-rule (==+ flo ...)
  (esc (make-coproducting (☯ flo) ...)))


(define-qi-syntax-rule (<> flo)
  (esc (coamp (☯ flo))))


(define-qi-syntax-rule (=< flo ...)
  (esc (->N: (☯ flo) ...)))


(define-qi-syntax-rule (let/cc flo)
  (esc (λ args (let/cc cc (apply (☯ flo) cc args)))))

(define-qi-syntax-rule (let/ec flo)
  (esc (λ args (let/ec ec (apply (☯ flo) ec args)))))


(define id (procedure-reduce-arity values 1))
(define bool->1+1 (☯ (~> (=< false? true?))))
