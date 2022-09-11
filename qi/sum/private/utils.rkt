#lang racket/base

(require racket/function
         syntax/parse/define)

(provide (all-defined-out))

(define (->boolean v) (and v #t))
(define (true?  arg) (eq? #t arg))
(define (false? arg) (eq? #f arg))
(define (thunk->list thk) (call-with-values thk list))
(define (list->thunk lst) (thunk (list->values lst)))
(define (list->values lst) (apply values lst))
(define-syntax-parse-rule (values->list body:expr ...+)
  (call-with-values (thunk body ...) list))

(define add (procedure-reduce-arity + 2))
(define sub (procedure-reduce-arity - 2))
(define mul (procedure-reduce-arity * 2))
(define div (procedure-reduce-arity / 2))


(define-values (compose1 compose)
  (let ()
    (define (id? f) (eq? values f))
    (define-syntax-rule (app1 E1 E2) (E1 E2))
    (define-syntax-rule (app* E1 E2) (call-with-values (lambda () E2) E1))
    (define-syntax-rule (mk-simple-compose app f g)
      (let-values
          ([(arity-mask) (procedure-arity-mask g)]
           [(required-kwds allowed-kwds) (procedure-keywords g)])
        (define composed
          (case arity-mask
            [(1) (λ ()    (app f (g)))]
            [(2) (λ (x)   (app f (g x)))]
            [(4) (λ (x y) (app f (g x y)))]
            [else
             (case-lambda
               [()    (app f (g))]
               [(x)   (app f (g x))]
               [(x y) (app f (g x y))]
               [args  (app f (apply g args))])]))
        (if (null? allowed-kwds)
            (if (eqv? arity-mask (procedure-arity-mask composed))
                composed
                (procedure-reduce-arity-mask composed arity-mask))
            (procedure-reduce-keyword-arity-mask
             (make-keyword-procedure
              (lambda (kws kw-args . xs)
                (app f (keyword-apply g kws kw-args xs)))
              composed)
             arity-mask required-kwds allowed-kwds 'composed 'racket))))
    (define-syntax-rule (can-compose* name n g f fs)
      (unless (null? (let-values ([(req _) (procedure-keywords g)]) req))
        (apply raise-argument-error 'name "procedure-with-no-required-keywords?"
               n f fs)))
    (define-syntax-rule (can-compose1 name n g f fs)
      (begin (unless (procedure-arity-includes? g 1)
               (apply raise-argument-error 'name "(any/c . -> . any/c)" n f fs))
             ;; need to check this too (see PR 11978)
             (can-compose* name n g f fs)))
    (define (pipeline1 f rfuns)
      ;; (very) slightly slower alternative:
      #;(if (null? rfuns)
            f
            (pipeline1 (let ([fst (car rfuns)])
                         (define (composed x) (fst (f x)))
                         composed)
                       (cdr rfuns)))
      (define composed
        (lambda (x)
          (let loop ([x x] [f f] [rfuns rfuns])
            (if (null? rfuns)
                (f x)
                (loop (f x) (car rfuns) (cdr rfuns))))))
      composed)
    (define (pipeline* f rfuns)
      ;; use the other composition style in this case, to optimize an
      ;; occasional arity-1 procedure in the pipeline
      (if (eqv? 2 (procedure-arity-mask f))
          ;; if `f' is single arity, then going in reverse they will *all* be
          ;; single arities
          (let loop ([f f] [rfuns rfuns])
            (if (null? rfuns)
                f
                (loop (let ([fst (car rfuns)])
                        (if (eqv? 2 (procedure-arity-mask fst))
                            (lambda (x) (fst (f x)))
                            (lambda (x) (app* fst (f x)))))
                      (cdr rfuns))))
          ;; otherwise, going in reverse means that they're all n-ary, which
          ;; means that the list of arguments will be built for each stage, so
          ;; to avoid that go forward in this case
          (let ([funs (reverse (cons f rfuns))])
            (let loop ([f (car funs)] [funs (cdr funs)])
              (if (null? funs)
                  f
                  (loop (let ([fst (car funs)])
                          (if (eqv? 2 (procedure-arity-mask f))
                              (if (eqv? 2 (procedure-arity-mask fst))
                                  (lambda (x) (f (fst x)))
                                  (lambda xs (f (apply fst xs))))
                              (if (eqv? 2 (procedure-arity-mask fst))
                                  (lambda (x) (app* f (fst x)))
                                  (lambda xs (app* f (apply fst xs))))))
                        (cdr funs)))))))
    (define-syntax-rule (mk name app can-compose pipeline mk-simple-compose)
      (define name
        (let ([simple-compose mk-simple-compose])
          (case-lambda
            [(f)
             (if (procedure? f) f (raise-argument-error 'name "procedure?" 0 f))]
            [(f g)
             (unless (procedure? f)
               (raise-argument-error 'name "procedure?" 0 f g))
             (unless (procedure? g)
               (raise-argument-error 'name "procedure?" 1 f g))
             (can-compose name 0 f f '())
             (simple-compose f g)]
            [() values]
            [(f0 . fs0)
             (let loop ([g f0] [fs fs0] [i 0] [rfuns '()])
               (unless (procedure? g)
                 (apply raise-argument-error 'name "procedure?" i f0 fs0))
               (if (pair? fs)
                   (begin (can-compose name i g f0 fs0)
                          (loop (car fs) (cdr fs) (add1 i) (cons g rfuns)))
                   (let* ([rfuns (remq* (list values) rfuns)]
                          [f (if (null? rfuns)
                                 values
                                 (pipeline (car rfuns) (cdr rfuns)))])
                     (simple-compose f g))))]))))
    (mk compose1 app1 can-compose1 pipeline1
        (lambda (f g) (mk-simple-compose app1 f g)))
    (mk compose  app* can-compose* pipeline*
        (lambda (f g)
          (cond
            [(id? f) g]
            [(eqv? 2 (procedure-arity-mask f))
             (mk-simple-compose app1 f g)]
            [else (mk-simple-compose app* f g)])))
    (values compose1 compose)))