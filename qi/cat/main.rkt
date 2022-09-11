#lang racket/base

(require "../sum.rkt")
(require "private/function.rkt")

(provide relay*
         tee
         amp
         (rename-out
          [relay* ==*]
          [tee    -<]
          [amp    ><])

         (all-from-out "../sum.rkt")
         (for-space qi
                    ==* -< ><
                    let/cc let/ec))


(define-qi-syntax-rule (==* flo ...)
  (esc (relay* (☯ flo) ...)))

(define-qi-syntax-rule (-< flo ...)
  (esc (tee (☯ flo) ...)))

(define-qi-syntax-rule (>< flo)
  (esc (amp (☯ flo))))


(define-qi-syntax-rule (let/cc flo)
  (esc (λ args (let/cc cc (apply (☯ flo) cc args)))))

(define-qi-syntax-rule (let/ec flo)
  (esc (λ args (let/ec ec (apply (☯ flo) ec args)))))
