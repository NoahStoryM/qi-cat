#lang racket/base

(require qi)
(require
 "utils.rkt"
 "function.rkt"
 "cofunction.rkt")

(provide bool->1+1
         id add sub mul div
         values->list list->values

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

         relay* relay+
         tee    cotee
         amp    coamp
         (rename-out
          [relay* ==*]
          [relay+ ==+]
          [tee    -<]
          [cotee  >-]
          [amp    ><]
          [coamp  <>]
          [->N:   =<])

         (all-from-out qi)
         (for-space qi
                    ~>  ~>>
                    ==+ ==*
                    >-  -<
                    <>  ><
                    =<
                    let/cc let/ec))


(define-qi-syntax-rule (~> flo ...)
  (esc (make-composed (☯ flo) ...)))

(define-qi-syntax-rule (~>> flo ...)
  (esc (make-composed (☯ flo) ...)))


(define-qi-syntax-rule (==+ flo ...)
  (esc (relay+ (☯ flo) ...)))
(define-qi-syntax-rule (==* flo ...)
  (esc (relay* (☯ flo) ...)))

(define-qi-syntax-rule (>- flo ...)
  (esc (cotee (☯ flo) ...)))
(define-qi-syntax-rule (-< flo ...)
  (esc (tee (☯ flo) ...)))

(define-qi-syntax-rule (<> flo)
  (esc (coamp (☯ flo))))
(define-qi-syntax-rule (>< flo)
  (esc (amp (☯ flo))))


(define-qi-syntax-rule (=< flo ...)
  (esc (->N: (☯ flo) ...)))


(define-qi-syntax-rule (let/cc flo)
  (esc (λ args (let/cc cc (apply (☯ flo) cc args)))))

(define-qi-syntax-rule (let/ec flo)
  (esc (λ args (let/ec ec (apply (☯ flo) ec args)))))


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
