#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/match
         racket/treelist)


;; *****************************************************************************
;; Arrow
;; *****************************************************************************
(provide (struct-out arrow) arrow= id? id src tgt ∙ ⨾)

(struct arrow (path source target) #:transparent)

(define arrow=
  (case-λ
    [(_) #t]
    [(f1 f2)
     (match-define (arrow p1 s1 t1) f1)
     (match-define (arrow p2 s2 t2) f2)
     (and (equal? p1 p2) (equal? s1 s2) (equal? t1 t2))]
    [(f1 f2 . f*) (and (arrow= f1 f2) (apply arrow= f2 f*))]))
(define (id? f)
  (and (arrow? f)
       (treelist-empty? (arrow-path f))
       (equal? (arrow-source f)
               (arrow-target f))))
(define (id n) (arrow (treelist) n n))
(define src (compose1 id arrow-source))
(define tgt (compose1 id arrow-target))
(define (∙ . f*) (apply ⨾ (reverse f*)))
(define ⨾
  (case-λ
    [(f) f]
    [(f . f*)
     (define s (arrow-source f))
     (for/fold ([p (arrow-path f)]
                [t (arrow-target f)]
                #:result (arrow p s t))
               ([f (in-list f*)])
       (define s (arrow-source f))
       (unless (equal? t s)
         (raise-argument-error '⨾ (~a t) (unquoted-printing-string s)))
       (values (treelist-append p (arrow-path f))
               (arrow-target f)))]))
