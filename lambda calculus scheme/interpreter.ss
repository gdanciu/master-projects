(require (lib "trace.ss"))

(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define keywords (list 'lambda 'def))

(define (variabila? var)
   (cond
     ((not (symbol? var) ) #f)
     ((member var keywords) #f)
     (else #t))
  )

(define (functie? fct)
   (cond 
     ((not (list? fct)) #f)
     ((not (eq? (car fct) 'lambda)) #f)
     (else #t)
     )
  )

(define (aplicatie? app)
  (cond
    ((not (list? app)) #f)
    ((not (= 2 (length app))) #f)
    (else #t)
    )
  )
(define (definitie? def)
   (cond 
     ((not (list? def)) #f)
     ((not (eq? (car def) 'def)) #f)
     (else #t)
     )
  )

(define (categorie var)
  (cond
    ((variabila? var) 'variabila)
    ((functie? var) 'functie)
    ((aplicatie? var) 'aplicatie)
    ((definitie? var) 'definitie)
    )
  )
 
 (define (free_vars exp)
   (cond
     ((null? exp) null)
     ((variabila? exp) (list exp))
     ((functie? exp)(remove* (list (cadr exp))(free_vars (caddr exp))))
     ((aplicatie? exp) (remove-duplicates (append (free_vars (car exp)) (free_vars (cadr exp)))))
    )
  )
 
  (define (bound_vars exp)
     (cond 
       ((null? exp) '())
       ((functie? exp) (append (list (cadr exp)) (bound_vars (caddr exp))))
       ((aplicatie? exp) (append (bound_vars (car exp)) (bound_vars (cadr exp))))
       (else '())
       )
   ) 
  
    (define replaceSym (λ (exp old new)
                       (cond
                         ((variabila? exp)
                          (if (eq? exp old)
                              new
                              exp
                              )
                          )
                         ((functie? exp)
                          (if (eq? old (cadr exp))
                              exp
                              (if (not (member (cadr exp) (free_vars new)))
                                  (list 'lambda (cadr exp) (replaceSym (caddr exp) old new))
                                  (let ((generat (gensym (cadr exp))))
                                    (list 'lambda generat (replaceSym (replaceSym (caddr exp) (cadr exp) generat) old new))
                                    )
                                  )
                              )
                          )
                         ((aplicatie? exp)
                          (list (replaceSym (car exp) old new) (replaceSym (cadr exp) old new))                          
                   )
                )
       )
    )

(define (resolvSymbol exp vars)
  (let ((valoare (assoc exp vars)))
           (if valoare
               (cdr valoare)
               exp
               )
           )
  )

(define (Evaluation exp vars)
  (cond ((variabila? exp)
         (resolvSymbol exp vars)
         )
        ((functie? exp) exp)
        ((aplicatie? exp)
         (let ((expStanga (car exp)) (expDreapta (cadr exp)))
           (cond ((functie? expStanga) (Evaluation (Reduce exp) vars))
                 ((aplicatie? expStanga) (Evaluation (list (Eval expStanga vars) expDreapta) vars))
                 ((variabila? expStanga) (Evaluation (list (resolvSymbol expStanga vars) expDreapta) vars))
                 )
           )
         )
        )
  )

;(Calltrace Evaluation)
(define (Reduce exp)
  (replaceSym (caddar exp) (cadar exp) (cadr exp))
  )
(define (Eval exp vars)
  (Reduce (list (Evaluation (car exp) vars) (cadr exp)))
  )


;(trace Evaluation)

(define (interpretor prog)
  (interpretorR prog ())
  )

(define (interpretorR prog vars)
  (cond ((null? prog) prog)
        ((definitie? (car prog)) (begin
                                   (let ((rez (Evaluation (caddar prog) vars)))
                                     (printf "~a~n" rez)
                                     (interpretorR (cdr prog) (cons (cons (cadar prog) rez) vars))
                                     )
                                   )
                                 )
        (else (begin 
                (printf "~a~n" (Evaluation (car prog) vars))
                (interpretorR (cdr prog) vars)
                )
              )
        )
  )

(define program
  '((def true_0 (lambda x (lambda y x)))
    (def false_0 (lambda x (lambda y y)))
 
    (def not_0 (lambda x ((x false_0) true_0)))
    (not_0 true_0)
    (not_0 false_0)
 
    (def and_0 (lambda x (lambda y ((x y) false_0))))
    ((and_0 true_0) true_0)
    ((and_0 true_0) false_0)
    ((and_0 false_0) true_0)
    ((and_0 false_0) false_0)
 
    (def or_0 (lambda x (lambda y ((x true_0) y))))
    ((or_0 true_0) true_0)
    ((or_0 true_0) false_0)
    ((or_0 false_0) true_0)
    ((or_0 false_0) false_0)))