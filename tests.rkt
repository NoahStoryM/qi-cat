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
         (☯                               ; 1 + 1 + 1 + 1
          (~> (==+ (>- #t #f) (>- #t #f)) ; B + B
              (==+ (-< #t id) (-< #f id)) ; (t × B) + (f × B)
              (>- _ _)))                  ; (t × B) ∪ (f × B) = B × B

         (☯
          (~> (==+ (~> (>- #t #f) (-< #t id))
                   (~> (>- #t #f) (-< #f id)))
              (>- _ _)))
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
         (☯                               ; B × B
          (~> (==* bool->1+1 id)          ; (1 + 1) × B
              (<<< 1)                     ; B + B
              (==+ bool->1+1 bool->1+1))) ; 1 + 1 + 1 + 1

         (☯                             ; B × B
          (~> (==* bool->1+1 bool->1+1) ; (1 + 1) × (1 + 1)
              (<<< 1)                   ; (1 + 1) + (1 + 1)
              (==+ 1< 2<)))             ; 1 + 1 + 1 + 1

         (☯                             ; B × B
          (~> (==* bool->1+1 bool->1+1) ; (1 + 1) × (1 + 1)
              (<<< 1)                   ; (1 + 1) + (1 + 1)
              (==+ (==+ _ ≂) _)))       ; 1 + 1 + 1 + 1

         (☯                             ; B × B
          (~> (==* bool->1+1 bool->1+1) ; (1 + 1) × (1 + 1)
              (<<< 1)                   ; (1 + 1) + (1 + 1)
              (<> (==+ _ ≂))))          ; 1 + 1 + 1 + 1
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

         (☯                                ;     num ∪ str
          (~> (esc (->N: number? string?)) ;       1 + 1
              (==+ 'number 'string)        ; 'number + 'string
              (>- _ _)))                   ; 'number ∪ 'string
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

         (☯                                        ; num ∪ str
          (~> (-< _ _)                             ; (num ∪ str) × (num ∪ str)
              (==* (esc (->N: number? string?)) _) ;   (1 + 1)   × (num ∪ str)
              (<<< 1)                              ; num + str
              (==+ - (string-append "-" _))        ; num + str
              (>- _ _)))                           ; num ∪ str
         ))])
  (check-equal? (t 123) -123)
  (check-equal? (t "0") "-0"))


(for ([i (in-list '(1 2 3 4))])
  (check-equal? (~> (i) (n< i) ▷ cdr (_)) i))
