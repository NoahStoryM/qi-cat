#lang racket/base

(require
 racket/list
 racket/match
 (only-in racket/math natural?)
 syntax/parse/define)

(provide (all-defined-out))


;; utils
(define (->boolean v) (not (not v)))
(define (true?  arg) (eq? #t arg))
(define (false? arg) (eq? #f arg))
(define (list->values ls) (apply values ls))
(define-syntax-parse-rule (values->list body:expr ...+)
  (call-with-values (λ () body ...) list))


;; covalues
(struct covalues (thk tag))
(define covalues-vals (λ (covals) ((covalues-thk covals))))

(define-values (n< 0< 1< 2< 3< 4< 5< 6< 7< 8< 9<)
  (let ()
    (define (n<: n)
      (define name (string->symbol (format "~a<" n)))
      (procedure-rename
       (match-lambda*
         [(list (covalues thk tag0))
          (define tag (+ tag0 (sub1 n)))
          (if (natural? tag)
              (covalues thk tag)
              (error name "~a isn't a natural number!" tag))]
         [vals
          (define tag (sub1 n))
          (if (natural? tag)
              (covalues (λ () (list->values vals)) tag)
              (error name "~a isn't a natural number!" tag))])
       name))
    (values
     (λ (n)
       (case n
         [(0) 0<]
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
     (n<: 0)
     (n<: 1)
     (n<: 2)
     (n<: 3)
     (n<: 4)
     (n<: 5)
     (n<: 6)
     (n<: 7)
     (n<: 8)
     (n<: 9))))

(define apply-n<
  (λ (t #:f0 [f0 values] . a*)
    (define-values (thk tag)
      (match a*
        [(list (covalues thk tag)) (values thk tag)]
        [_ (values (λ () (list->values a*)) 0)]))
    (call-with-values thk (apply make-composed f0 (make-list tag t)))))


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
     #:when (zero? (procedure-arity thk))
     (covalues thk tag)]
    #;[(? procedure? thk)
       #:when (zero? (procedure-arity thk))
       (covalues thk 0)]))
(define ▷
  (match-lambda
    [(covalues thk tag) (cons thk tag)]
    #;[thk (cons thk 0)]))


;; composed
(struct composed (coarity result-coarity f)
  #:property prop:procedure
  (λ (self . args)
    (apply (composed-f self) args)))
(define make-composed
  (λ f*
    (cond
      [(null? f*) values]
      [else
       (define coarity (procedure-coarity (car f*)))
       (let ([f* (reverse f*)])
         (define result-coarity
           (procedure-result-coarity (car f*)))
         (define f (apply compose f*))
         (composed coarity result-coarity f))])))


;; coprocedure
(struct coprocedure (f*)
  #:property prop:procedure
  (λ (self . args) (error '≂ "Can't call ≂")))
(define ≂ (coprocedure '()))
(define power ≂)
(define ≂? (λ (arg) (and (coprocedure? arg) (null? (coprocedure-f* arg)))))
(define power? ≂?)

(struct coproducting coprocedure ()
  #:property prop:procedure
  (λ (self . args)
    (define f* (coprocedure-f* self))
    (when (null? f*) (error '≂ "Can't call ≂"))
    (define-values (thk coarity)
      (match args
        [(list (covalues thk tag)) (values thk (add1 tag))]
        [_ (values (λ () (list->values args)) 1)]))
    (let loop ([coarity coarity] [tag1 0] [f* f*])
      (when (null? f*) (error 'call "tag out of bound"))
      (define f (car f*))
      (define n (procedure-coarity f))
      (cond
        [(≂? f)
         (loop coarity (add1 tag1) (cdr f*))]
        [(> coarity n)
         (loop (- coarity n) (+ tag1 (procedure-result-coarity f)) (cdr f*))]
        [else
         (match (values->list
                 (call-with-values
                  (if (= n 1)
                      thk
                      (λ () (covalues thk (sub1 coarity))))
                  f))
           [(list (covalues thk tag)) (covalues thk (+ tag tag1))]
           [vals (covalues (λ () (list->values vals)) tag1)])]))))

(struct copairing coprocedure ()
  #:property prop:procedure
  (λ (self . args)
    (define f* (coprocedure-f* self))
    (when (null? f*) (error '≂ "Can't call ≂"))
    (define-values (thk coarity)
      (match args
        [(list (covalues thk tag)) (values thk (add1 tag))]
        [_ (values (λ () (list->values args)) 1)]))
    (let loop ([coarity coarity] [f* f*])
      (when (null? f*) (error 'call "tag out of bound"))
      (define f (car f*))
      (define n (procedure-coarity f))
      (cond
        [(≂? f) (loop coarity (cdr f*))]
        [(> coarity n) (loop (- coarity n) (cdr f*))]
        [else
         (call-with-values
          (if (= n 1)
              thk
              (λ () (covalues thk (sub1 coarity))))
          f)]))))


;; (coprocedure)   = ≂
;; (coprocedure f) = f
(define-values (make-coproducting make-copairing)
  (let ()
    (define (make: pred make)
      (λ f*
        (case (length f*)
          [(0) ≂]
          [(1) (make (car f*))]
          [else
           (make
               (flatten
                (for/list ([f (in-list f*)])
                  (cond
                    [(≂? f) ≂]
                    [(pred f) (coprocedure-f* f)]
                    [else f]))))])))
    (values (make: coproducting? coproducting)
            (make: copairing?    copairing))))


;; coarity
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
    (cond
      [(coprocedure? f)
       (define f* (coprocedure-f* f))
       (define coarity* (map procedure-coarity f*))
       (apply + coarity*)]
      #;[(N->? (length (N->-args f)))]
      [(composed? f) (composed-coarity f)]
      [else 1])))

(define procedure-result-coarity
  (λ (f)
    (cond
      [(≂? f) 1]
      [(coprocedure? f)
       (define f* (coprocedure-f* f))
       (define result-coarity* (map procedure-result-coarity f*))
       ;; g : X -> A + B
       ;; h : Y + Z -> C
       (cond
         [(coproducting? f)
          ;; f = g + h : X + Y + Z -> A + B + C
          (apply + result-coarity*)]
         [(copairing? f)
          ;; f = <g | h> : X + Y + Z -> (A + B) ∪ (C + 0)
          (apply max 0 result-coarity*)])]
      [(->N? f) (length (->N-preds f))]
      [(composed? f) (composed-result-coarity f)]
      [else 1])))


;; disjoint union -> tag union
(struct ->N (preds)
  #:property prop:procedure
  (λ (self . args)
    (define preds (->N-preds self))
    (define n (index-where preds (λ (pred) (apply pred args))))
    (covalues (λ () (values)) n)))
(define ->N: (λ preds (if (null? preds) ≂ (->N preds))))

;; tag union -> disjoint union
#;(begin
    (struct N-> (args)
      #:property prop:procedure
      (λ (self arg) (list-ref args (covalues-tag arg))))
    (define N->: (λ args (if (null? args) ≂ (N-> args)))))


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
