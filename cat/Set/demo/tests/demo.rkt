#lang racket

(require "../main.rkt"
         racket/treelist
         rackunit
         variant)

(displayln 'Start)

(define-syntax-rule (check-variant= e1 e2)
  (check-equal?
   (let*-variant ([(#:tag [n 0] . v1) e1]) (cons v1 n))
   (let*-variant ([(#:tag [n 0] . v2) e2]) (cons v2 n))))


(test-case "Type tests"
  (check-true (type= '(+ (+ a b) (+ c d)) '(+ a b c d)))
  (check-true (type= '(× a (+ b c)) '(+ (× a b) (× a c))))
  (check-true (type= '(× (+ b c) a) '(+ (× b a) (× c a))))
  (check-true (type= '(× (+ a b) (+ c d)) '(+ (× a c) (× a d) (× b c) (× b d))))
  (check-true (type= '(× (+ a b) y (+ c d)) '(+ (× a y c) (× a y d) (× b y c) (× b y d))))
  (check-true (type= '(+ a 0 b) '(+ a b)))
  (check-true (type= '(× a 1 b) '(× a b)))
  (check-true (type= 0 '(+) '(× 0) '(× a 0) '(× 0 a) '(× a 0 b)))
  (check-true (type= 1 '(×)))
  (check-true (type= 2 '(+ (×) (×))))
  (check-true (type= 5 '(+ 3 2) '(+ 2 3)))
  (check-true (type= 6 '(× 2 3) '(× 3 2))))

(define a (ann : (+ Any 1)))
(define b (ann : (+ (× Any Any) 1)))
(define c (ann : (× Any Any)))
(define d (ann : Any))

(define f
  (ann
   (λ (#:tag [n 0] . a*)
     (match* (a* n)
       [((list a) 0) (variant a a #:tag 0)]
       [((list  ) 1) (variant     #:tag 1)]))
   (→ (+ Any 1) (+ (× Any Any) 1))))

(define g
  (ann
   (λ (#:tag [n 0] . a*)
     (match* (a* n)
       [((list a0 a1) 0) (variant #:tag 0 a0 a1)]
       [((list      ) 1) (variant #:tag 0  0  0)]))
   (→ (+ (× Any Any) 1) (× Any Any))))

(define h
  (ann
   (λ (#:tag [n 0] . a*)
     (match* (a* n)
       [((list a0 a1) 0) (variant #:tag 0 a0)]))
   (→ (× Any Any) Any)))

(test-case "Arrow tests"
  (check-pred id? a)
  (check-pred id? b)
  (check-pred id? c)
  (check-pred id? d)
  (check-variant= (f 'a #:tag 0) (variant 'a 'a))
  (check-variant= (f 'a) (variant 'a 'a #:tag 0))
  (check-variant= (f #:tag 1) (variant #:tag 1))
  (check-exn exn:fail:contract? (λ () (∙ f h))))

(test-case "Category tests"
  ;; Existence of composition
  (check-true (arrow= b (tgt f) (src g)))
  (check-true (arrow= a (src (∙ g f)) (src f)))
  (check-true (arrow= c (tgt (∙ g f)) (tgt g)))

  ;; Associativity of composition
  (check-true (arrow= (∙ h g f) (∙ (∙ h g) f) (∙ h (∙ g f))))

  ;; Existence of identity arrows
  (check-true (arrow= a (src a) (tgt a)))

  ;; Composition and identity arrows
  (check-true (arrow= f (∙ f (src f)) (∙ (tgt f) f))))

(test-case "Product tests"
  (define a×b×c (==× a b c))
  (define b×c×d (==× b c d))
  (define f×g×h (==× f g h))

  (check-true (arrow= a×b×c (src f×g×h)))
  (check-true (arrow= b×c×d (tgt f×g×h)))

  (check-variant= (f×g×h 1 2 3 4 5) (variant 1 1 2 3 4))
  (check-variant= (f×g×h 1 2 3 #:tag 1) (variant 1 1 0 0 2))
  (check-variant= (f×g×h 1 2 3 4 #:tag 2) (variant 1 2 3 #:tag 1))
  (check-variant= (f×g×h 1 2 #:tag 3) (variant 0 0 1 #:tag 1)))
