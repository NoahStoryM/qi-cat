#lang racket/base

(require racket/list
         racket/match
         racket/function
         racket/math)

(require "utils.rkt")

(provide (all-defined-out))


(define split-input
  (λ (n arity*)
    (define report-arity-error
      (λ ()
        (raise-arguments-error
         'split-input
         (string-append
          "arity mismatch;\n"
          " the expected number of arguments does not match the given number")
         "given" n)))
    (define len (length arity*))
    (define-values (m a*)
      (for/fold ([m n] [a* '()])
                ([arity (in-list arity*)]
                 [i (in-naturals)])
        (if (= 1 (- len i))
            (match arity
              [(? exact-nonnegative-integer? n)
               (values (- m n) a*)]
              [(or (arity-at-least n)
                   (list* n _))
               (values (- m n) `([,i ,n ,arity] . ,a*))])
            (match arity
              [(? exact-nonnegative-integer? n)
               (values (- m n) a*)]
              [(arity-at-least 0)
               (values (- m 1) `([,i  1 ,arity] . ,a*))]
              [(or (arity-at-least n)
                   (list* 0 (arity-at-least n))
                   (list* 0 n _)
                   (list* n _))
               (values (- m n) `([,i ,n ,arity] . ,a*))]))))
    (unless (>= m 0)
      (report-arity-error))
    (apply list-set*
      arity*
      (for/fold ([m m] [pairs '()] #:result (if (zero? m) pairs (report-arity-error)))
                ([a (in-list a*)])
        (define-values (i n arity) (apply values a))
        (cond
          [(zero? m)
           (values 0 (list* i n pairs))]
          [(arity-includes? arity (+ n m))
           (values 0 (list* i (+ n m) pairs))]
          [(arity-at-least? arity)
           (report-arity-error)]
          [(list? arity)
           (match (last arity)
             [(? arity-at-least?)
              (report-arity-error)]
             [(? exact-nonnegative-integer? j)
              (values (- m j) (list* i (+ n j) pairs))])])))))

(define (~map f vs)
  (define-values (arity 0?)
    (match (procedure-arity f)
      [0 (values 0 #t)]
      [(? exact-positive-integer? n) (values n #f)]
      [(arity-at-least 0) (values 1 #f)] ; (~> () (>< 1) ▽) should return '().
      [(arity-at-least n) (values n #f)]
      [(list* 0 1 _) (values 1 #f)]
      [(list* 0 (or (arity-at-least n) n) _) (values n #t)]
      [(list* n _) (values n #f)]))
  (cond
    [(and 0? (null? vs)) (values->list (f))]
    [(zero? (remainder (length vs) arity))
     (case arity
       [(1)
        (let loop ([vs vs])
          (match vs
            ['() '()]
            [(list* v0 vs)
             (append (values->list (f v0))
                     (loop vs))]))]
       [(2)
        (let loop ([vs vs])
          (match vs
            ['() '()]
            [(list* v0 v1 vs)
             (append (values->list (f v0 v1))
                     (loop vs))]))]
       [else
        (let loop ([vs vs])
          (cond
            [(null? vs) '()]
            [else
             (define-values (vs0 vs*) (split-at vs arity))
             (append (values->list (apply f vs0))
                     (loop vs*))]))])]
    [else (raise-arguments-error
           '~map (format "needs ~ax values" arity)
           "given" vs)]))

(define arity-sum
  (case-lambda
    [() '()]
    [(a) a]
    [(a b)
     (match* (a b)
       [((? natural?) (? natural?))
        (+ a b)]
       [((arity-at-least a) (? natural?))
        (arity-at-least (+ a b))]
       [((? natural?) (arity-at-least b))
        (arity-at-least (+ a b))]
       [((arity-at-least a) (arity-at-least b))
        (arity-at-least (+ a b))]
       [(_ (? null?)) a]
       [((? null?) _) b]
       [((? list?) _)
        (normalize-arity
         (flatten
          (for/list ([a (in-list a)])
            (arity-sum a b))))]
       [(_ (? list?))
        (normalize-arity
         (flatten
          (for/list ([b (in-list b)])
            (arity-sum a b))))])]
    [a* (foldl arity-sum '() a*)]))
(define arity-union
  (case-lambda
    [() '()]
    [(a) a]
    [a* (normalize-arity (flatten a*))]))

(define 1->1 (global*))
(define *->1 (const*))


(define relay*
  (case-lambda
    [() 1->1]
    [(f0) f0]
    [f*
     (define fs (remq* (list 1->1) f*))
     (case (length fs)
       [(0) 1->1]
       [(1) (car fs)]
       [else
        (define arity* (map procedure-arity fs))
        (define arity (apply arity-sum arity*))
        (define compiled-relay*-flow
          (λ args
            (define args*
              (for/fold ([a '()] [a* args] #:result (reverse a))
                        ([i (in-list (split-input (length args) arity*))])
                (define-values (v v*) (split-at a* i))
                (values (cons v a) v*)))
            (apply values
              (append*
               (for/list ([f (in-list fs)]
                          [args (in-list args*)])
                 (values->list
                  (match* ((procedure-arity f) args)
                    [(0 '()) (f)]
                    [(1 `(,v0)) (f v0)]
                    [(2 `(,v0 ,v1)) (f v0 v1)]
                    [(_ _) (apply f args)])))))))
        (if (equal? arity (arity-at-least 0))
            compiled-relay*-flow
            (procedure-reduce-arity compiled-relay*-flow arity))])]))

(define tee
  (case-lambda
    [() *->1]
    [(f0) f0]
    [f*
     (define fs (remq* (list *->1) f*))
     (case (length fs)
       [(0) *->1]
       [(1) (car fs)]
       [else
        (define arity* (map procedure-arity fs))
        (define arity (apply arity-union arity*))
        (define compiled-tee-flow
          (λ args
            (apply values
              (append*
               (for/list ([f (in-list fs)])
                 (values->list (apply f args)))))))
        (if (equal? arity (arity-at-least 0))
            compiled-tee-flow
            (procedure-reduce-arity compiled-tee-flow arity))])]))

(define amp
  (λ (f)
    (define compiled-amp-flow
      (λ args
        (apply values
          (~map f args))))
    compiled-amp-flow))
