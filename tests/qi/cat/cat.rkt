#lang racket

(require qi/cat
         rackunit)


(for ([factorial
       (in-list
        (list
         (λ (n)
           (define loop
             (λ (p m)
               (cond
                 [(=  m 0) p]
                 [(>= m 1) (loop (* p m) (sub1 m))])))
           (loop 1 n))

         (let ()
           (define factorial
             (let ([factorial (λ _ (apply factorial _))])
               (☯ ; n + p × m + p
                 (>- (~>             ; n
                       (-< 1 _)      ; p × m  (p = 1, m = n)
                       +1< factorial)
                     (~>                                ;   p × m
                       (==* _ (-< _ (=< (>= 1) (= 0)))) ;   p × m   × (1 + 1)
                       (<<< 3)                          ;   p × m   +  p × m
                       (==+ (-< * (~> 2> sub1)) 1>)     ; p*m × m-1 +  p
                       +1< factorial)
                     _ ; p
                     ))))
           factorial)

         (let ()
           (define-flow (factorial n)  ; n + p × m + p
             (>- (~> (-< 1 _) +1< factorial)
                 (~>                                ;   p × m
                   (==* _ (-< _ (=< (>= 1) (= 0)))) ;   p × m   × (1 + 1)
                   (<<< 3)                          ;   p × m   +  p × m
                   (==+ (-< * (~> 2> sub1)) 1>)     ; p*m × m-1 +  p
                   +1< factorial)
                 _))
           factorial)

         (☯
           (~> (let/cc (==* _ _ 1))           ; loop × n × res
               (if (~> 2> zero?)
                   3>
                   (~> (==* _ (-< _ _) _)     ; loop × n × n × res
                       (==* (-< _ _) sub1 *)  ; loop × loop × (sub1 n) × (* n res)
                       (_ _ _ _)))))

         (☯
           (~> (let/cc (==* _ _ 1))           ; loop × n × res
               (==* _ (-< _ (=< zero? #t)) _) ; loop × n × (1 + 1) × res
               (<<< 3)                        ; loop × n × res + loop × n × res
               (==+ 3>                        ;            res + (loop loop (sub1 n) (* n res))
                    (~> (==* _ (-< _ _) _)
                        (==* (-< _ _) sub1 *)
                        (_ _ _ _)))
               (fanin 2)))))])

  (check-eq? (factorial 0) 1)
  (check-eq? (factorial 1) 1)
  (check-eq? (factorial 2) 2)
  (check-eq? (factorial 3) 6)
  (check-eq? (factorial 4) 24)
  (check-eq? (factorial 5) 120)
  (check-eq? (factorial 6) 720)
  (check-eq? (factorial 7) 5040)
  (check-eq? (factorial 8) 40320)
  (check-eq? (factorial 9) 362880))


(let ()
  ;;; Env : 1 + Var × (Box Val) × Env


  ;; empty-environment : * -> Env
  (define empty-environment (☯ (~> ⏚ 1<)))

  ;; extend-environment : Var × Val × Env -> Env
  (define extend-environment (☯ (~> (==* id box id) 2<)))

  ;; lookup-variable-value : Var × Env -> Val
  (define lookup-variable-value
    (let ([lookup-variable-value (λ _ (apply lookup-variable-value _))])
      (☯
        (~> (<<< 2) ; Var + Var × Var × (Box Val) × Env
            (>- (error
                 'lookup-variable-value
                 "no value found for key\n  key: ~a" _)
                (~> (-< 1> (==* (~> eq? bool->1+1) _)) ; Var × (1 + 1) × (Box Val) × Env
                    (<<< 2)
                    (>- (~> (==* id ⏚ id) lookup-variable-value)
                        (~> 2> unbox))))))))


  (define env
    (~> () empty-environment
        (-< 'a 0 _) extend-environment
        (-< 'b 1 _) extend-environment))

  (check-eq? (~> (env) (-< 'a _) lookup-variable-value) 0)
  (check-eq? (~> (env) (-< 'b _) lookup-variable-value) 1))
