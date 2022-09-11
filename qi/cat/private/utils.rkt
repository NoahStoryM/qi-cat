#lang racket/base

(require racket/function
         "../../sum/private/utils.rkt")
(provide (all-defined-out)
         (all-from-out "../../sum/private/utils.rkt"))

(define const*
  (case-lambda
    [()    (thunk* (values))]
    [(c)   (thunk* c)]
    [(c d) (thunk* (values c d))]
    [c*    (thunk* (apply values c*))]))

(define global (λ (c) (thunk c)))
(define global*
  (case-lambda
    [()    (thunk (values))]
    [(c)   (thunk c)]
    [(c d) (thunk (values c d))]
    [c*    (thunk (apply values c*))]))


(define (list-update* l . pairs)
  (unless (list? l)
    (apply raise-argument-error 'list-update* "list?" 0 l pairs))
  (unless (even? (length pairs))
    (raise-arguments-error
     'list-update*
     (format "expected ~a, but received ~a"
             "an even number of association elements"
             "an odd number of association elements")
     "association elements"
     pairs))
  (define len (length l))
  (let check ([p pairs] [i 1])
    (unless (null? p)
      (define j (car p))
      (unless (exact-nonnegative-integer? j)
        (apply raise-argument-error 'list-update* "exact-nonnegative-integer?" i l pairs))
      (unless (< j len)
        (raise-range-error 'list-update* "list" "" j l 0 (sub1 len)))
      (define q (cdr p))
      (define f (car q))
      (unless (and (procedure? f)
                   (procedure-arity-includes? f 1))
        (apply raise-argument-error 'list-update* "(-> any/c any/c)" (+ 1 i) l pairs))
      (check (cdr q) (+ 2 i))))
  (define cache (apply hasheq pairs))
  (let loop ([l l] [i 0] [j (hash-count cache)])
    (cond
      [(zero? j) l]
      [(hash-ref cache i #f)
       => (λ (f) (cons (f (car l)) (loop (cdr l) (add1 i) (sub1 j))))]
      [else (cons (car l) (loop (cdr l) (add1 i) j))])))

(define (list-set* l . pairs)
  (unless (list? l)
    (apply raise-argument-error 'list-set* "list?" 0 l pairs))
  (unless (even? (length pairs))
    (raise-arguments-error
     'list-set*
     (format "expected ~a, but received ~a"
             "an even number of association elements"
             "an odd number of association elements")
     "association elements"
     pairs))
  (define len (length l))
  (let check ([p pairs] [i 1])
    (unless (null? p)
      (define j (car p))
      (unless (exact-nonnegative-integer? j)
        (apply raise-argument-error 'list-set* "exact-nonnegative-integer?" i l pairs))
      (unless (< j len)
        (raise-range-error 'list-set* "list" "" j l 0 (sub1 len)))
      (check (cddr p) (+ 2 i))))
  (define cache (apply hasheq pairs))
  (let loop ([l l] [i 0] [j (hash-count cache)])
    (cond
      [(zero? j) l]
      [(hash-has-key? cache i)
       (cons (hash-ref cache i) (loop (cdr l) (add1 i) (sub1 j)))]
      [else (cons (car l) (loop (cdr l) (add1 i) j))])))
