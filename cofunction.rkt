#lang racket/base

(require
 racket/list
 racket/match
 racket/function
 racket/math
 syntax/parse/define)

(provide (all-defined-out)
         (rename-out
          [identity id]
          [0->* ≂]))


;; utils
(define (->boolean v) (and v #t))
(define (true?  arg) (eq? #t arg))
(define (false? arg) (eq? #f arg))
(define (list->values ls) (apply values ls))
(define-syntax-parse-rule (values->list body:expr ...+)
  (call-with-values (λ () body ...) list))


;; covalues
(struct covalues (thk tag))
(define covalues-vals (λ (covals) ((covalues-thk covals))))

(define-values (n< 1< 2< 3< 4< 5< 6< 7< 8< 9<)
  (let ()
    (define (n<: n)
      (define name (string->symbol (format "~a<" n)))
      (procedure-rename
       (λ args
         (define-values (thk tag)
           (values (λ () (list->values args)) (sub1 n)))
         (if (natural? tag)
             (covalues thk tag)
             (error name "~a isn't a natural number!" tag)))
       name))
    (values
     (λ (n)
       (case n
         [(1) 1<]
         [(2) 2<]
         [(3) 3<]
         [(4) 4<]
         [(5) 5<]
         [(6) 6<]
         [(7) 7<]
         [(8) 8<]
         [(9) 9<]
         [else (n<: n)]))
     (n<: 1)
     (n<: 2)
     (n<: 3)
     (n<: 4)
     (n<: 5)
     (n<: 6)
     (n<: 7)
     (n<: 8)
     (n<: 9))))

(define-values (+n<
                0<
                +1< +2< +3< +4< +5< +6< +7< +8< +9<
                -1< -2< -3< -4< -5< -6< -7< -8< -9<)
  (let ()
    (define (+n<: n)
      (define name
        (string->symbol
         (format (if (> n 0) "+~a<" "~a<") n)))
      (procedure-rename
       (λ args
         (define-values (thk tag)
           (match args
             [(list (covalues thk tag0)) (values thk (+ tag0 n))]
             [_ (values (λ () (list->values args)) n)]))
         (if (natural? tag)
             (covalues thk tag)
             (error name "~a isn't a natural number!" tag)))
       name))
    (values
     (λ (n)
       (case n
         [( 0)  0<]
         [( 1) +1<]
         [( 2) +2<]
         [( 3) +3<]
         [( 4) +4<]
         [( 5) +5<]
         [( 6) +6<]
         [( 7) +7<]
         [( 8) +8<]
         [( 9) +9<]
         [(-1) -1<]
         [(-2) -2<]
         [(-3) -3<]
         [(-4) -4<]
         [(-5) -5<]
         [(-6) -6<]
         [(-7) -7<]
         [(-8) -8<]
         [(-9) -9<]
         [else (+n<: n)]))
     (+n<:  0)
     (+n<:  1)
     (+n<:  2)
     (+n<:  3)
     (+n<:  4)
     (+n<:  5)
     (+n<:  6)
     (+n<:  7)
     (+n<:  8)
     (+n<:  9)
     (+n<: -1)
     (+n<: -2)
     (+n<: -3)
     (+n<: -4)
     (+n<: -5)
     (+n<: -6)
     (+n<: -7)
     (+n<: -8)
     (+n<: -9))))

(define f0->f
  (case-lambda
    [(t) (f0->f values t)]
    [(f0 t)
     (λ a*
       (define-values (thk tag)
         (match a*
           [(list (covalues thk tag))
            (values (λ () (call-with-values thk f0)) tag)]
           [_ (values (λ () (apply f0 a*)) 0)]))
       (call-with-values thk (apply compose (make-list tag t))))]))


#;(begin
    #| A
    =  A + 0 + 0 + ... + 0
    != 0 + 0 + ... + 0 + A
    |#

    (define make-covalues
      (λ (thk tag)
        (if (= 0 tag)
            (thk)
            (covalues thk tag))))
    (define get-covalues-tag
      (λ (obj)
        (if (covalues? obj)
            (covalues-tag obj)
            0)))
    (define get-covalues-thk
      (λ (obj)
        (if (covalues? obj)
            (covalues-thk obj)
            obj))))


(define ◁
  (match-lambda
    [(cons (? procedure? thk) (? natural? tag))
     #:when (arity-includes? (procedure-arity thk) 0)
     (covalues thk tag)]
    #;[(? procedure? thk)
       #:when (arity-includes? (procedure-arity thk) 0)
       (covalues thk 0)]))
(define ▷
  (match-lambda
    [(covalues thk tag) (cons thk tag)]
    #;[thk (cons thk 0)]))


;; coprocedure
(struct coprocedure (coarity result-coarity))

(struct composed coprocedure (f)
  #:property prop:procedure (struct-field-index f))
(define make-composed
  (case-lambda
    [() values]
    [(f0) f0]
    [(f0 . f*)
     (let ([f* (reverse (remq* (list values) f*))])
       (cond
         [(null? f*) f0]
         [else
          (define coarity (procedure-coarity f0))
          (define result-coarity (procedure-result-coarity (car f*)))
          (define f (compose (apply compose f*) f0))
          (composed coarity result-coarity f)]))]))


(struct coproducting coprocedure (f*)
  #:property prop:procedure
  (λ (self . args)
    (define f* (coproducting-f* self))
    (when (null? f*) (error '0->0 "Can't call 0->0"))
    (define-values (thk coarity)
      (match args
        [(list (covalues thk tag)) (values thk (add1 tag))]
        [_ (values (λ () (list->values args)) 1)]))
    (let loop ([coarity coarity] [tag1 0] [f* f*])
      (when (null? f*) (error 'coproducting "tag out of bound"))
      (define f (car f*))
      (define m (procedure-coarity f))
      (define n (procedure-result-coarity f))
      (if (> coarity m)
          (loop (- coarity m) (+ tag1 n) (cdr f*))
          (match (values->list
                  (call-with-values
                   (if (= m 1)
                       thk
                       (λ () (covalues thk (sub1 coarity))))
                   f))
            [(list (covalues thk tag)) (covalues thk (+ tag tag1))]
            [vals (covalues (λ () (list->values vals)) tag1)])))))
(define 0->0 (coproducting 0 0 '()))
(define 0->0? (λ (arg) (eq? arg 0->0)))
(define make-coproducting
  (case-lambda
    [() 0->0]
    [(f0) f0]
    [f*
     (define fs
       (flatten
        (for/list ([f (in-list f*)])
          (cond
            [(eq? 0->0 f) '()]
            [(coproducting? f) (coproducting-f* f)]
            [else f]))))
     (case (length fs)
       [(0) 0->0]
       [(1) (car fs)]
       [else
        (define-values (coarity result-coarity)
          (for/fold ([coarity 0] [result-coarity 0])
                    ([f (in-list fs)])
            (values (coarity-sum coarity (procedure-coarity f))
                    (coarity-sum result-coarity (procedure-result-coarity f)))))
        (coproducting coarity result-coarity fs)])]))

(struct copairing coprocedure (f*)
  #:property prop:procedure
  (λ (self . args)
    (define f* (copairing-f* self))
    (when (null? f*) (error '0->* "Can't call 0->*"))
    (define-values (thk coarity)
      (match args
        [(list (covalues thk tag)) (values thk (add1 tag))]
        [_ (values (λ () (list->values args)) 1)]))
    (let loop ([coarity coarity] [f* f*])
      (when (null? f*) (error 'call "tag out of bound"))
      (define f (car f*))
      (define m (procedure-coarity f))
      (define n (procedure-result-coarity f))
      (if (> coarity m)
          (loop (- coarity m) (cdr f*))
          (call-with-values
           (if (= m 1)
               thk
               (λ () (covalues thk (sub1 coarity))))
           f)))))
(define 0->* (copairing 0 1 '()))
(define 0->*? (λ (arg) (eq? arg 0->*)))
(define make-copairing
  (case-lambda
    [() 0->*]
    [(f0) f0]
    [f*
     (define fs
       (flatten
        (for/list ([f (in-list f*)])
          (cond
            [(eq? 0->* f) '()]
            [(copairing? f) (copairing-f* f)]
            [else f]))))
     (case (length fs)
       [(0) 0->*]
       [(1) (car fs)]
       [else
        (define-values (coarity result-coarity)
          (for/fold ([coarity 0] [result-coarity 0])
                    ([f (in-list fs)])
            (values (coarity-sum   coarity (procedure-coarity f))
                    (coarity-union result-coarity (procedure-result-coarity f)))))
        (copairing coarity result-coarity fs)])]))

;; disjoint union -> tag union
(struct ->N coprocedure (pred*)
  #:property prop:procedure
  (λ (self . args)
    (define pred* (->N-pred* self))
    (when (null? pred*) (error '*->0 "Can't call *->0"))
    (define n (index-where pred* (λ (pred) (apply pred args))))
    (covalues (λ () (values)) n)))
(define *->0 (->N 1 0 '()))
(define *->0? (λ (arg) (eq? arg *->0)))
(define ->N:
  (case-lambda
    [() *->0]
    [pred* (->N 1 (length pred*) pred*)]))


;; coarity
(define coarity-sum   (curry  +  0))
(define coarity-union (curry max 0))
#;(begin
    (struct	coarity-at-least (value)
      #:extra-constructor-name make-coarity-at-least)
    (define procedure-coarity?
      (λ (v)
        (or (natural? v)
            (coarity-at-least? v)
            (and (list? v) (ormap natural? v)))))

    (define normalize-coarity
      (λ (coarity)
        (if (list? coarity)
            (let ([coarity (remove-duplicates coarity)])
              (case (length coarity)
                [(0) 0]
                [(1) (car coarity)]
                [else (sort coarity <)]))
            coarity)))

    (define coarity=?
      (λ (a b)
        (equal?
         (normalize-coarity a)
         (normalize-coarity b))))

    (define coarity-includes?
      (λ (a b)
        (if (list? a)
            (->boolean
             (if (list? b)
                 (andmap (λ (b) (member b a)) b)
                 (member b a)))
            (eqv? a b)))))


(define procedure-coarity
  (λ (f)
    (if (coprocedure? f)
        (coprocedure-coarity f)
        1)))

(define procedure-result-coarity
  (λ (f)
    (if (coprocedure? f)
        (coprocedure-result-coarity f)
        1)))


;; distributive law
(define >>>
  (λ (coarity)
    (match-lambda
      [(covalues thk tag)
       (call-with-values
        thk
        (λ args
          (list->values
           (list-update
            args
            (sub1 coarity)
            (match-lambda
              #;[(covalues thk tag0)
                 (covalues thk (+ tag tag0))]
              [arg (covalues (λ () arg) tag)])))))])))
(define <<<
  (λ (coarity)
    (λ args
      (define-values (head tail)
        (split-at args (sub1 coarity)))
      (match (car tail)
        [(covalues thk tag)
         (call-with-values
          thk
          (λ args
            (match (append head args (cdr tail))
              #;[(list (covalues thk tag0))
                 (covalues thk (+ tag tag0))]
              [vals (covalues (λ () (list->values vals)) tag)])))]))))


;; higher-order
(define <>
  (λ (f)
    (define n (procedure-coarity f))
    (define m (procedure-result-coarity f))
    (match-lambda*
      [(list (covalues thk tag))
       (define-values (coa1 coa0)
         (quotient/remainder (add1 tag) n))
       (define-values (tag1 tag0)
         (values (sub1 coa1) (sub1 coa0)))
       (match (values->list (apply f (list (covalues thk tag0))))
         [(list (covalues thk tag))
          (covalues thk (+ (* m tag1) tag))]
         [vals
          (covalues thk (* m tag1))])]
      [vals (apply f vals)])))
(define coamp <>)

(define fanin (λ (n) (apply make-copairing (make-list n values))))
