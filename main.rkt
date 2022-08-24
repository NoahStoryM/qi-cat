#lang racket/base

(require qi)
(require "cofunction.rkt")

(provide bool->1+1
         id add sub mul div

         maybe
         map-maybe
         just? nothing?
         option->maybe maybe->option
         list->maybe maybe->list

         n< 1< 2< 3< 4< 5< 6< 7< 8< 9<
         +n< 0<
         +1< +2< +3< +4< +5< +6< +7< +8< +9<
         -1< -2< -3< -4< -5< -6< -7< -8< -9<
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


(define bool->1+1 (☯ (~> (=< false? true?))))

(define add (procedure-reduce-arity + 2))
(define sub (procedure-reduce-arity - 2))
(define mul (procedure-reduce-arity * 2))
(define div (procedure-reduce-arity / 2))


;; Maybe
(define ((maybe . a*) f)
  (☯
    (~> (-< _ (=< nothing? just?))
        (<<< 2)
        (==+ (~> (gen a*) △) f))))

(define just?    (☯ (>- #f #t)))
(define nothing? (☯ (>- #t #f)))

(define option->maybe (☯ (~> (-< _ (=< not #t)) (<<< 2) (==+ ⏚ _))))
(define maybe->option (☯ (>- #f _)))

(define list->maybe (☯ (~> (-< _ (=< null? #t)) (<<< 2) (==+ ⏚ car))))
(define maybe->list (☯ (>- '() list)))

(define map-maybe (λ (f) (☯ (~> △ (>< (~> f (fanin 2))) ▽))))
