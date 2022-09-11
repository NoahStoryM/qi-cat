#lang racket/base

(require qi)
(require "private/utils.rkt"
         "private/cofunction.rkt")

(provide bool->1+1
         id add sub mul div
         values->list list->values
         covalues?

         procedure-coarity
         procedure-result-coarity

         maybe
         map-maybe
         just? nothing?
         option->maybe maybe->option
         list->maybe maybe->list

         n< 1< 2< 3< 4< 5< 6< 7< 8< 9<
         +n< (rename-out [0< +0<] [0< -0<])
         +1< +2< +3< +4< +5< +6< +7< +8< +9<
         -1< -2< -3< -4< -5< -6< -7< -8< -9<

         f0->f
         ≂
         ◁ ▷
         >>> <<<
         fanin

         relay+
         cotee
         coamp
         (rename-out
          [relay+ ==+]
          [cotee  >-]
          [coamp  <>]
          [->N:   =<])

         (all-from-out qi)
         (for-space qi
                    ~>  ~>>
                    ==+ >- <>
                    =<))


(define-qi-syntax-rule (~> flo ...)
  (esc (make-composed (☯ flo) ...)))

(define-qi-syntax-rule (~>> flo ...)
  (esc (make-composed (☯ flo) ...)))


(define-qi-syntax-rule (==+ flo ...)
  (esc (relay+ (☯ flo) ...)))

(define-qi-syntax-rule (>- flo ...)
  (esc (cotee (☯ flo) ...)))

(define-qi-syntax-rule (<> flo)
  (esc (coamp (☯ flo))))


(define-qi-syntax-rule (=< flo ...)
  (esc (->N: (☯ flo) ...)))


(define bool->1+1 (☯ (~> (=< false? true?))))

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
