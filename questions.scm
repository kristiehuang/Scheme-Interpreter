(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all f rs) 
      (if (null? rs) (list (cons f nil))
      (map (lambda (x) (append (list f) x)) rs)
      )
)

(define (zip pairs)
  ; (define keys nil)
  ; (define values nil)
  ; ; for list in pairs:
  ; ;   (define keys (cons (car list) keys))
  ; ;   (define values (cons (cadr list) values))
  ; ; keys, values
  ; (begin
  ;   (map (lambda (list) (
  ;                       (define keys (cons (car list) keys))
  ;                       (define values (cons (cadr list) values))
  ;         )
  ;             ) pairs)
  ;   (list keys values)


      (list (map car pairs) (map cadr pairs))

)

;; Problem 16
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
  (define (enum-iter i s)
    (if (null? s)
        '()
        (cons (list i (car s)) (enum-iter (+ i 1) (cdr s)))
    ))
  (enum-iter 0 s)
  )
  ; END PROBLEM 16

  (define (list-change total denoms)
  ; BEGIN PROBLEM 17
    (cond
      ((null? denoms) '())
      ((< total 0) '())
      ((eq? total 0) (cons (cons (car denoms) nil) nil))
      ((> (car denoms) total) (list-change total (cdr denoms)))
      ((= (car denoms) total) (append (list-change (- total (car denoms)) denoms) (list-change total (cdr denoms))))
      (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
    )
  )
    ; END PROBLEM 17


;; Problem 18
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (cons form 
                (cons (map (lambda (x) (let-to-lambda x)) params) 
                      (map (lambda (x) (let-to-lambda x)) body))
            )
           ; END PROBLEM 18
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (cons (cons 'lambda (cons (car (zip (let-to-lambda values) )) 
                                      (let-to-lambda body))
                                  ) 
                  (car (cdr (zip (let-to-lambda values) )))
            )
           ; END PROBLEM 18
           ))
        (else
         ; BEGIN PROBLEM 18
         (map (lambda (x) (let-to-lambda x)) expr)
         ; END PROBLEM 18
         )))
