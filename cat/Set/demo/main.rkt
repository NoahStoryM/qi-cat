#lang racket/base

(require (for-syntax racket/base syntax/parse)
         "../../demo.rkt"
         racket/case
         racket/list
         racket/match
         racket/treelist
         variant)

(provide (all-from-out "../../demo.rkt"))


;; *****************************************************************************
;; Utilities
;; *****************************************************************************

(define natural? exact-nonnegative-integer?)
(define (atom? v)
  (and (symbol? v)
       (case/eq v
         [(× + → ←) #f]
         [else #t])))

(define (dims->vols dim*)
  (define len (vector-length dim*))
  (define vol* (make-vector len 1))
  (for ([dim (in-vector dim* (sub1 len) 0 -1)]
        [i (in-range (- len 2) -1 -1)])
    (define vol (vector-ref vol* (add1 i)))
    (vector-set! vol* i (* dim vol)))
  vol*)

(define (ravel-ids id* dim*)
  (define len (vector-length dim*))
  (for/sum ([id (in-vector id* (sub1 len) -1 -1)]
            [vol (in-vector (dims->vols dim*))])
    (* id vol)))

(define (unravel-pos pos dim*)
  (define len (vector-length dim*))
  (define id* (make-vector len))
  (for/fold ([pos pos])
            ([vol (in-vector (dims->vols dim*))]
             [i (in-range len)])
    (let-values ([(id pos) (quotient/remainder pos vol)])
      (vector-set! id* i id)
      pos))
  id*)


;; *****************************************************************************
;; Type
;; *****************************************************************************
(provide type? type= normalize-type get-arity get-coarity)

(define (type? v)
  (match v
    [(or `(× ,tp* ...) `(+ ,tp* ...)) (andmap type? tp*)]
    [(or `(→ ,s ,t) `(← ,t ,s)) (and (type? s) (type? t))]
    [_ (or (atom? v) (natural? v))]))

(define type=
  (case-λ
    [(_) #t]
    [tp*
     (let ([tp* (map normalize-type tp*)])
       (for/fold ([tp1 (car tp*)] [r #t] #:result r)
                 (#:when r [tp2 (in-list (cdr tp*))])
         (values tp2 (equal? tp1 tp2))))]))

(define (flatten-ops op tp*)
  (append*
   (for/list ([tp (in-list tp*)])
     (match tp
       [`(,(== op eq?) ,tp* ...) tp*]
       [_ (list tp)]))))

(define (process-add tp*)
  (let* ([tp* (map normalize-type tp*)]
         [tp* (flatten-ops '+ tp*)]
         [tp* (remove* '((+)) tp*)])
    (cond
      [(null? tp*) '(+)]
      [(null? (cdr tp*)) (car tp*)]
      [else `(+ . ,tp*)])))

(define (process-mul tp*)
  (let* ([tp* (map normalize-type tp*)]
         [tp* (flatten-ops '× tp*)]
         [tp* (if (member '(+) tp*)
                  '((+))
                  (remove* '((×)) tp*))])
    (cond
      [(null? tp*) '(×)]
      [(null? (cdr tp*)) (car tp*)]
      [else
       (match/values (splitf-at tp* (match-λ [`(+ ,_ ...) #f] [_ #t]))
         [(_ '()) `(× . ,tp*)]
         [(before `((+ ,term* ...) . ,after))
          (process-add
           (for/list ([term (in-list term*)])
             `(× ,@before ,term . ,after)))])])))

(define normalize-type
  (match-λ
    [(? atom? tp) tp]
    [(? natural? tp)
     (case/eqv tp
       [(0) '(+)]
       [(1) '(×)]
       [else (cons '+ (make-list tp '(×)))])]
    [`(+ ,tp* ...) (process-add tp*)]
    [`(× ,tp* ...) (process-mul tp*)]
    [(or `(→ ,s ,t) `(← ,t ,s))
     `(→ ,(normalize-type s) ,(normalize-type t))]))

(define (unsafe-get-arity tp n)
  (let ([tp
         (match tp
           [`(+ ,tp* ...)
            (define l (length tp*))
            (unless (< n l)
              (raise-range-error 'get-arity "tag" "" n 0 (sub1 l)))
            (list-ref tp* n)]
           [_
            (unless (zero? n)
              (raise-range-error 'get-arity "tag" "" n 0 0))
            tp])])
    (match tp
      [`(× ,tp* ...) (length tp*)]
      [_ 1])))
(define (get-arity tp n) (unsafe-get-arity (normalize-type tp) n))

(define (unsafe-get-coarity tp)
  (match tp
    [`(+ ,tp* ...) (length tp*)]
    [_ 1]))
(define (get-coarity tp) (unsafe-get-coarity (normalize-type tp)))


;; *****************************************************************************
;; Function
;; *****************************************************************************
(provide (struct-out function) ann ~>)

(define (apply-path p* #:tag [n 0] . a*)
  ((for/fold ([thk (λ () (apply/variant variant a* #:tag n))])
             ([p (in-treelist p*)])
     (λ () (call-with-variant thk p)))))
(define (apply-function f #:tag [n 0] . a*)
  (match-define (arrow p s t) f)
  (define arity (unsafe-get-arity s n))
  (unless (= arity (length a*))
    (define name
      (let ([name (object-name (treelist-first p))])
        (if (symbol? name) name 'apply-function)))
    (apply raise-arity-error name arity a*))
  (apply apply-path p #:tag n a*))

(struct function arrow ()
  #:guard (λ (p s t _) (values p (normalize-type s) (normalize-type t)))
  #:property prop:procedure apply-function
  #:transparent)

(define-syntax ann
  (syntax-parser
    #:datum-literals (: → ←)
    [(_ p (~or* (→ s t) (← t s)))
     #'(function (treelist p) `s `t)]
    [(_ : e)
     #'(let ([tp `e]) (function (treelist) tp tp))]))

(define ~>
  (case-λ
    [(f) f]
    [f*
     (match-define (arrow p s t) (apply ⨾ f*))
     (function p s t)]))


;; *****************************************************************************
;; Product Type
;; *****************************************************************************
(provide ⏚ |1| proj ==× -< #;fanout)

(define (*->1 . _) (values))
(define (⏚ s) (ann *->1 (→ ,s 1)))
(define |1| (ann : 1))

(define (proj s n)
  (match s
    [`(× ,n* ...)
     (define l (length n*))
     (unless (< n l)
       (raise-range-error 'proj "×" "" n 0 (sub1 l)))
     (define t (list-ref n* n))
     (ann (λ v* (list-ref v* n)) (→ ,s ,t))]
    [`(+ ,_ ...)
     (raise-range-error 'proj "×" "" n 0 -1)]
    [_
     (unless (zero? n)
       (raise-range-error 'proj "×" "" n 0 0))
     (ann : ,s)]))

(define (-< f . f*)
  (define s (arrow-source f))
  (define t*
    (for/list ([f (in-list (cons f f*))])
      (unless (equal? s (arrow-source f))
        (raise-argument-error '-< (~a s) (unquoted-printing-string (arrow-source f))))
      t))
  (define t (normalize-type `(× . ,t*)))
  (match (remove* (list (⏚ s)) (cons f f*) arrow=)
    ['()
     (unless (equal? t '(×))
       (raise-argument-error '-< (~a t) (unquoted-printing-string'(×))))
     #;(⏚ s) f]
    [`(,f) f]
    [f*
     (define p
       (treelist
        (λ arg*
          ;; TODO
          )))
     (arrow p s t)]))

(define (==× . f*)
  (if (member |0| f* arrow=)
      |0|                                   ; 0 = 0 × f = f × 0
      (match (remove* (list |1|) f* arrow=) ; f = 1 × f = f × 1
        ['() |1|]
        [`(,f) f]
        [f*
         (define len (length f*))
         (define-values (p* s* t*)
           (for/lists (p* s* t*) ([f (in-list f*)])
             (match-define (arrow p s t) f)
             (values p s t)))
         (define s (normalize-type `(× . ,s*)))
         (define t (normalize-type `(× . ,t*)))
         (define p
           (if (andmap treelist-empty? p*)
               (treelist)               ; Identity function
               (treelist
                (λ (#:tag [#;input-tag it 0] . arg*)
                  ;; Check inputs
                  (define arity (unsafe-get-arity s it))
                  (unless (= (length arg*) arity)
                    (apply raise-arity-error '==× arity arg*))

                  ;; Split inputs
                  (define #;input-tags it*
                    (unravel-pos it
                     (for/vector #:length len ([s (in-list s*)])
                       (get-coarity s))))
                  (define #;component-arguments comp-arg* (make-vector len))
                  (for/fold ([arg* arg*])
                            ([s (in-list s*)]
                             [it (in-vector it*)]
                             [i (in-range len)])
                    (let-values ([(comp-arg arg*) (split-at arg* (unsafe-get-arity s it))])
                      (vector-set! comp-arg* i comp-arg)
                      arg*))

                  ;; Merge outputs
                  (define #;output-tags ot* (make-vector len))
                  (define res*
                    (for/fold ([res* '()])
                              ([p (in-list p*)]
                               [it (in-vector it*)]
                               [comp-arg (in-vector comp-arg*)]
                               [i (in-range len)])
                      (define-variant (#:tag [#;output-tag ot 0] . #;component-result comp-res)
                        (apply apply-path p #:tag it comp-arg))
                      (vector-set! ot* i ot)
                      (append res* comp-res)))
                  (define #;output-tag ot
                    (ravel-ids ot*
                     (for/vector #:length len ([t (in-list t*)])
                       (get-coarity t))))

                  ;; Check outputs
                  (define result-arity (unsafe-get-arity t ot))
                  (unless (= (length res*) result-arity)
                    (apply raise-result-arity-error '==× result-arity #f res*))

                  (apply/variant variant #:tag ot res*)))))
         (function p s t)])))


#|
********************************************************************************
Sum Type
********************************************************************************
|#
(provide ⎓ |0| #;inj #;==+ #;>- #;fanin)

(define 0->* (case-λ))
(define (⎓ t) (ann 0->* (→ 0 ,t)))
(define |0| (ann : 0))


#|
********************************************************************************
Exponential Type
********************************************************************************
|#
#;(provide ev clos _)
