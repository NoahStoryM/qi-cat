#lang racket

(require "main.rkt"
         rackunit)


(let ()
  ;;  t : #t
  ;;  f : #f
  ;;  B : t ∪ f
  ;;  1 : (values)
  ;;  d : (A×B)+(A×C) -> A×(B+C)
  ;; ¬d : A×(B+C) -> (A×B)+(A×C)
  (define  d (>>> 2))
  (define ¬d (<<< 2))
  (define string->symbol/list
    (☯                                     ; str × (t ∪ f) = str × B
     (~> (==* id bool->1+1)                ; str × (1 + 1)
         ¬d                                ; str + str
         (==+ string->symbol string->list) ; sym + lst
         (>- _ _))))                       ; sym ∪ lst
  (check-equal? (values->list (string->symbol/list "hello" #t)) '(hello))
  (check-equal? (values->list (string->symbol/list "hello" #f)) '((#\h #\e #\l #\l #\o))))


(for ([f
       (in-list
        (list
         (☯                               ;   1 + 1 + 1 + 1
          (~> (==+ (>- #t #f) (>- #t #f)) ;       B + B
              (==+ (-< #t id) (-< #f id)) ; (t × B) + (f × B)
              (>- _ _)))                  ; (t × B) ∪ (f × B) = B × B

         (☯
          (~> (==+ (~> (>- #t #f) (-< #t id))
                   (~> (>- #t #f) (-< #f id)))
              (>- _ _)))

         (☯
          (~> (==+ (~> (>- #t #f) (-< #t id))
                   (~> (>- #t #f) (-< #f id)))
              (fanin 2)))
         ))])

  (check-equal? (values->list (~> () 1< f)) '(#t #t))
  (check-equal? (values->list (~> () 2< f)) '(#t #f))
  (check-equal? (values->list (~> () 3< f)) '(#f #t))
  (check-equal? (values->list (~> () 4< f)) '(#f #f))

  (check-equal? (values->list (~> () (==+ _ ≂ ≂ ≂) f)) '(#t #t))
  (check-equal? (values->list (~> () (==+ ≂ _ ≂ ≂) f)) '(#t #f))
  (check-equal? (values->list (~> () (==+ ≂ ≂ _ ≂) f)) '(#f #t))
  (check-equal? (values->list (~> () (==+ ≂ ≂ ≂ _) f)) '(#f #f)))


(for ([¬f
       (in-list
        (list
         (☯                               ;       B × B
          (~> (==* bool->1+1 id)          ; (1 + 1) × B
              (<<< 1)                     ;       B + B
              (==+ bool->1+1 bool->1+1))) ;   1 + 1 + 1 + 1

         (☯                             ;       B × B
          (~> (==* bool->1+1 bool->1+1) ; (1 + 1) × (1 + 1)
              (<<< 1)                   ; (1 + 1) + (1 + 1)
              (==+ 1< 2<)))             ;   1 + 1 + 1 + 1

         (☯                             ;       B × B
          (~> (==* bool->1+1 bool->1+1) ; (1 + 1) × (1 + 1)
              (<<< 1)                   ; (1 + 1) + (1 + 1)
              (==+ (==+ _ ≂) _)))       ;   1 + 1 + 1 + 1

         (☯                             ;       B × B
          (~> (==* bool->1+1 bool->1+1) ; (1 + 1) × (1 + 1)
              (<<< 1)                   ; (1 + 1) + (1 + 1)
              (<> (==+ _ ≂))))          ;   1 + 1 + 1 + 1
         ))])

  (define *n* (☯ (~> ¬f (>- 1 2 3 4))))
  (check-equal? (*n* #t #t) 1)
  (check-equal? (*n* #t #f) 2)
  (check-equal? (*n* #f #t) 3)
  (check-equal? (*n* #f #f) 4)

  (define *and* (☯ (~> ¬f (>- #t #f #f #f))))
  (check-equal? (*and* #t #t) #t)
  (check-equal? (*and* #t #f) #f)
  (check-equal? (*and* #f #t) #f)
  (check-equal? (*and* #f #f) #f))


(for ([t
       (in-list
        (list
         (λ (arg)
           (cond
             [(number? arg) 'number]
             [(string? arg) 'string]))

         (☯                             ;     num ∪ str
          (~> (=< number? string?)      ;       1 + 1
              (==+ 'number 'string)     ; 'number + 'string
              (fanin 2)))               ; 'number ∪ 'string
         ))])
  (check-equal? (t 123) 'number)
  (check-equal? (t "0") 'string))


(for ([t
       (in-list
        (list
         (λ (arg)
           (cond
             [(number? arg) (- arg)]
             [(string? arg) (string-append "-" arg)]))

         (☯                                 ;     num ∪ str
          (~> (-< (=< number? string?) _)   ; (1 + 1) × (num ∪ str)
              (<<< 1)                       ;     num + str
              (==+ - (string-append "-" _)) ;     num + str
              (fanin 2)))                   ;     num ∪ str
         ))])
  (check-equal? (t 123) -123)
  (check-equal? (t "0") "-0"))


(for ([min
       (in-list
        (list
         (λ (arg1 arg2)
           (cond
             [(<= arg1 arg2) arg1]
             [(>  arg1 arg2) arg2]))

         (☯                             ;          real × real
          (~> (-< (=< <= >) _)          ;       (1 + 1) × (real × real)
              (<<< 1)                   ; (real × real) + (real × real)
              (==+ 1> 2>)               ;          real + real
              (fanin 2)))               ;          real ∪ real
         ))])
  (check-equal? (min  123  123)  123)
  (check-equal? (min -123  123) -123)
  (check-equal? (min  123 -123) -123))


(for ([i (in-list '(1 2 3 4))])
  (check-equal? (~> (i) (n< i) ▷ cdr) (sub1 i))
  (check-equal? (~> (i) (n< i) ▷ car (_)) i))


(for ([i (in-range 10)])
  (define factorial-1
    (λ (n)
      (define loop
        (λ (p m)
          (cond
            [(=  m 0) p]
            [(>= m 1) (loop (* p m) (sub1 m))])))
      (loop 1 n)))
  (define-flow (factorial-2 n)  ; n + p × m + p
    (>- (~> (-< 1 _) 2< factorial-2)
        (~>                               ;   p × m
         (==* _ (-< _ (=< (>= 1) (= 0)))) ;   p × m   × (1 + 1)
         (<<< 3)                          ;   p × m   +  p × m
         (==+ (-< * (~> 2> sub1)) 1>)     ; p*m × m-1 +  p
         2< factorial-2)
        _))
  (check-equal? (factorial-1 i) (factorial-2 i)))
