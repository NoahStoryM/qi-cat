#lang racket

(require qi+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Covalues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Covalues is just the Values tagged with natural numbers.
|#

;;; n<
(~> (123) 1<) ; #<covalues>
(~> (123) 2<) ; #<covalues>
(~> (123) 3<) ; #<covalues>

;;; ◁ and ▷
(~> (123) 1< ▷ cdr)     ; 0
(~> (123) 1< ▷ car (_)) ; 123

(~> (123) 2< ▷ cdr)     ; 1
(~> (123) 2< ▷ car (_)) ; 123

(~> (123) 3< ▷ cdr)     ; 2
(~> (123) 3< ▷ car (_)) ; 123

(~> ((cons (λ () 123) 0)) ◁) ; #<covalues>
(~> ((cons (λ () 123) 1)) ◁) ; #<covalues>
(~> ((cons (λ () 123) 2)) ◁) ; #<covalues>

(~> (123) 1< 5< ▷ cdr) ; 4 = (1 - 1) + (5 - 1)
(~> (123) 2< 5< ▷ cdr) ; 5 = (2 - 1) + (5 - 1)
(~> (123) 3< 5< ▷ cdr) ; 6 = (3 - 1) + (5 - 1)

#|
Procedures can be regarded as the morphisms between Covalues.
|#

;;; quotient/remainder : Integer × Integer -> Integer × Integer
(procedure-arity          quotient/remainder) ; 2
(procedure-coarity        quotient/remainder) ; 1
(procedure-result-coarity quotient/remainder) ; 1


#|
For f : A -> X and g : B -> Y, there are
 f+g  = (==+ f g) : A + B -> X + Y
<f|g> = (>-  f g) : A + B -> X ∪ Y
|#

;;; add1 : Number -> Number
;;; sub1 : Number -> Number
(procedure-coarity        add1) ; 1
(procedure-result-coarity add1) ; 1

(procedure-coarity        sub1) ; 1
(procedure-result-coarity sub1) ; 1

;;;  add1 + sub1  : Number + Number -> Number + Number
;;; <add1 | sub1> : Number + Number -> Number
(procedure-coarity        (☯ (==+ add1 sub1))) ; 2
(procedure-result-coarity (☯ (==+ add1 sub1))) ; 2

(procedure-coarity        (☯ (>-  add1 sub1))) ; 2
(procedure-result-coarity (☯ (>-  add1 sub1))) ; 1

(~> (0) 1< (==+ add1 sub1) ▷ cdr)     ;  0
(~> (0) 1< (==+ add1 sub1) ▷ car (_)) ;  1

(~> (0) 2< (==+ add1 sub1) ▷ cdr)     ;  1
(~> (0) 2< (==+ add1 sub1) ▷ car (_)) ; -1

(~> (0) 1< (>-  add1 sub1))           ;  1
(~> (0) 2< (>-  add1 sub1))           ; -1

(☯ (fanout 3)) ; (☯ (-< _ _ _))
(☯ (fanin  3)) ; (☯ (>- _ _ _))

;;; !!! WARNING !!!
(procedure-coarity        (λ _ (apply (☯ (==+ add1 sub1)) _))) ; 1
(procedure-result-coarity (λ _ (apply (☯ (==+ add1 sub1)) _))) ; 1

(~> (0) 1< (esc (λ _ (apply (☯ (==+ add1 sub1)) _))) ▷ cdr)     ;  0
(~> (0) 1< (esc (λ _ (apply (☯ (==+ add1 sub1)) _))) ▷ car (_)) ;  1

(~> (0) 2< (esc (λ _ (apply (☯ (==+ add1 sub1)) _))) ▷ cdr)     ;  1
(~> (0) 2< (esc (λ _ (apply (☯ (==+ add1 sub1)) _))) ▷ car (_)) ; -1


#|
We'll use
1 to represent the identity element of Values,
0 to represent the identity element of Covalues.

A
= 1 × A = A × 1
= 0 + A = A + 0
|#

;;; _ : * -> *

;;; We can use ==* and ⏚ to do the same things as n>.
;;; ⏚ : * -> 1

;; (==* _ ⏚ ⏚) : (A×B×C -> A) = (A×B×C -> A×1×1)
;; (==* ⏚ _ ⏚) : (A×B×C -> B) = (A×B×C -> 1×B×1)
;; (==* ⏚ ⏚ _) : (A×B×C -> C) = (A×B×C -> 1×1×C)

(~> (1 2 3) 1>) ; 1
(~> (1 2 3) 2>) ; 2
(~> (1 2 3) 3>) ; 3

(~> (1 2 3) (==* _ ⏚ ⏚)) ; 1
(~> (1 2 3) (==* ⏚ _ ⏚)) ; 2
(~> (1 2 3) (==* ⏚ ⏚ _)) ; 3


;;; Similarly:
;;; We can use ==+ and ≂ to do the same things as n<.
;;; ≂ : 0 -> *

;; (==+ _ ≂ ≂) : (A -> A+B+C) = (A+0+0 -> A+B+C)
;; (==+ ≂ _ ≂) : (B -> A+B+C) = (0+B+0 -> A+B+C)
;; (==+ ≂ ≂ _) : (C -> A+B+C) = (0+0+C -> A+B+C)

(~> (123) 1< ▷ cdr) ; 0
(~> (123) 2< ▷ cdr) ; 1
(~> (123) 3< ▷ cdr) ; 2

(~> (123) (==+ _ ≂ ≂) ▷ cdr) ; 0
(~> (123) (==+ ≂ _ ≂) ▷ cdr) ; 1
(~> (123) (==+ ≂ ≂ _) ▷ cdr) ; 2


;;; Differences
(procedure-coarity        (☯ 2<)) ; 1
(procedure-result-coarity (☯ 2<)) ; 1

(procedure-coarity        (☯ (==+ ≂ _ ≂))) ; 1
(procedure-result-coarity (☯ (==+ ≂ _ ≂))) ; 3


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Distributive Law
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Products distribute over Sums.

;;; For example, (A×B)+(A×C) and A×(B+C) are isomorphic:

(define  d (>>> 2)) ; (A×B)+(A×C) -> A×(B+C)
(define ¬d (<<< 2)) ; A×(B+C) -> (A×B)+(A×C)

;; (>>> n) : move the tag from input to its nth element.
;; (<<< n) : move the tag from the nth element to input.

(~> ("A" "B") 1< d)
(~> ("A" "B") ; "A" × "B"
    (==+ _ ≂) ; ("A" × "B") + ("A" × "C")
    (ε (~> displayln))             ; display #<covalues>
    (ε (~> ▷ cdr displayln))       ; display 0
    (ε (~> ▷ car (_) ▽ displayln)) ; display (A B)
    d         ; "A" × ("B" + "C")
    (ε (~> 1> displayln))              ; display A
    (ε (~> 2> displayln))              ; display #<covalues>
    (ε (~> 2> ▷ cdr displayln))        ; display 0
    (ε (~> 2> ▷ car (_) ▽ displayln))) ; display (B)

(~> ("A" "B") (==* _ 1<) ¬d)
(~> ("A" "B")         ; "A" × "B"
    (==* _ (==+ _ ≂)) ; "A" × ("B" + "C")
    (ε (~> 1> displayln))             ; display A
    (ε (~> 2> displayln))             ; display #<covalues>
    (ε (~> 2> ▷ cdr displayln))       ; display 0
    (ε (~> 2> ▷ car (_) ▽ displayln)) ; display (B)
    ¬d                ; ("A" × "B") + ("A" × "C")
    (ε (~> displayln))              ; display #<covalues>
    (ε (~> ▷ cdr displayln))        ; display 0
    (ε (~> ▷ car (_) ▽ displayln))) ; display (A B)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Conditional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1 + 1 + ... + 1 (N) has n elements, so it's isomorphic to
;;; the types with n elements.

;;; For example, Boolean is isomorphic to 1 + 1:
(define (true?  arg) (eq? #t arg))
(define (false? arg) (eq? #f arg))

(define 1+1->bool (☯ (>- #t #f)))
(define bool->1+1 (☯ (=< true? false?)))

(~> (#t) bool->1+1 ▷ cdr)       ; 0
(~> (#t) bool->1+1 ▷ car (_) ▽) ; '()

(~> (#f) bool->1+1 ▷ cdr)       ; 1
(~> (#f) bool->1+1 ▷ car (_) ▽) ; '()

(~> () 1< 1+1->bool) ; #t
(~> () 2< 1+1->bool) ; #f

;;; "a" ∪ "b" ∪ "c" is isomorphic to 1 + 1 + 1:
(define 1+1+1->abc (☯ (>- "a" "b" "c")))
(define abc->1+1+1 (☯ (=< (equal? "a") (equal? "b") (equal? "c"))))

(~> ("c") abc->1+1+1 ▷ cdr)       ; 2
(~> ("c") abc->1+1+1 ▷ car (_) ▽) ; '()

(~> () 1< 1+1+1->abc) ; "a"
(~> () 2< 1+1+1->abc) ; "b"
(~> () 3< 1+1+1->abc) ; "c"


;;; 1 + 1 + ... + 1 can be used for conditional:
(define min
  (λ (x y)
    (cond
      [(<= x y) x]
      [(>  x y) y])))

(define min
  (☯                    ;       x × y
   (~> (-< (=< <= >) _) ; (1 + 1) × (x × y)
       (<<< 1)          ; (x × y) + (x × y)
       (==+ 1> 2>)      ;       x + y
       (fanin 2))))     ;       x ∪ y


#|
The values stored in covalues can also be a covalues,
so that (1 + 1) + (1 + 1) isn't the same as 1 + 1 + 1 + 1.
This is because in some cases it's difficult to figure out
which tag should be used.

For example:
(define ¬d (<<< 1)) ; (A+B)×(C+D) -> A×(C+D) + B×(C+D)

It seems that ¬d should convert the input to
the covalues tagged with 1 if the 1st element is of type B.
But if A is 1, it seems that the input should be tagged with 2:
(1+B)×(C+D) -> 1×(C+D) + B×(C+D) = C + D + B×(C+D)

|#

;;; To convert (1 + 1) + ... to 1 + 1 + ..., we can use (==+ _ ≂).

(define *and*
  (☯                             ;       B × B
   (~> (==* bool->1+1 bool->1+1) ; (1 + 1) × (1 + 1)
       (<<< 1)                   ; (1 + 1) + (1 + 1)
       (<> (==+ _ ≂))            ;   1 + 1 + 1 + 1
       (>- #t #f #f #f))))       ;   t ∪ f ∪ f ∪ f
(*and* #t #t) ; #t
(*and* #t #f) ; #f
(*and* #f #t) ; #f
(*and* #f #f) ; #f

(define *or*
  (☯                             ;       B × B
   (~> (==* bool->1+1 bool->1+1) ; (1 + 1) × (1 + 1)
       (<<< 1)                   ; (1 + 1) + (1 + 1)
       (<> (==+ _ ≂))            ;   1 + 1 + 1 + 1
       (>- #t #t #t #f))))       ;   t ∪ t ∪ t ∪ f
(*or* #t #t) ; #t
(*or* #t #f) ; #t
(*or* #f #t) ; #t
(*or* #f #f) ; #f


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Iteration and Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; It's common to define a local loop procedure for iteration.
;;; For example:
(define factorial
  (λ (n)
    (define loop
      (λ (p m)
        (cond
          [(=  m 0) p]
          [(>= m 1) (loop (* p m) (sub1 m))])))
    (loop 1 n)))

;;; With covalues, there is no need to define loop:
(define-flow (factorial n) ; n + p × m + p
  (>- (~>            ; n
       (-< 1 _)      ; p × m  (p = 1, m = n)
       2< factorial)
      (~>                               ;   p × m
       (==* _ (-< _ (=< (>= 1) (= 0)))) ;   p × m   × (1 + 1)
       (<<< 3)                          ;   p × m   +  p × m
       (==+ (-< * (~> 2> sub1)) 1>)     ; p*m × m-1 +  p
       2< factorial)
      _ ; p
      ))
