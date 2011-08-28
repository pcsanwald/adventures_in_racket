
; This chapter is all about implementing the scheme language in scheme itself.
; pretty cool!

; helper functions
(define (first p) (car p))
(define (second p)(car (cdr p)))
(define (build a b) (cons a (cons b (quote()))))
(define (third p) (car (cdr (cdr p))))
(define (atom? a) (not (list? a)))

(define lookup-in-entry
  (lambda (name entry error_function)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          error_function)))

(define lookup-in-entry-help
  (lambda (name first second error_function)
    (cond 
      ((null? first) (error_function name))
      ((eq? name (car first)) (car second))
      (else (lookup-in-entry-help name (cdr first) (cdr second) error_function))
      )))

;(lookup-in-entry 'foo (list (list 'bar 'foo) (list 'bar_val 'foo_val)) (lambda (x) x))
;(lookup-in-entry 'non-existent (list (list 'bar 'foo) (list 'bar_val 'foo_val)) (lambda (x) x))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table error-function)
    (cond
      ((null? table) (error-function name))
      (else
       (lookup-in-entry name (car table) (lambda (name) (lookup-in-table name (cdr table) error-function)))))
      ))

;(lookup-in-table 'foo (list (list (list 'bar 'foo) (list 'bar_val 'foo_val))) (lambda (x) x))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      ( else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const) 
      ((eq? e (quote car)) *const) 
      ((eq? e (quote cdr)) *const) 
      ((eq? e (quote null?)) *const) 
      ((eq? e (quote eq?)) *const) 
      ((eq? e (quote atom?)) *const) 
      ((eq? e (quote zero?)) *const) 
      ((eq? e (quote add1)) *const) 
      ((eq? e (quote sub1)) *const) 
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *apply)))
      (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t ) #t)
      ((eq? e #f ) #f)
      (else (build (quote primitive) e)))))

(define text-of second)

(define *quote 
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table (lambda (name) (car (quote ()))))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

; below is a partial implementation of cond.

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))
(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

; define an expression
(define some-expr (list 'cond (list 'coffee 'klatsch) (list 'else 'party)))

; we're going to make a table, which is a list of entries. first,
; make some entries.

(define entry-1 (list (list 'coffee) (list #t)))
(define entry-2 (list (list 'klatsch 'party) (list 5 6)))

; now make a table.
(define some-table (build entry-1 entry-2))

;(lookup-in-table 'klatsch some-table (lambda (name) (list 'oh-no-no-no)))

;(*cond some-expr some-table)

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote()))
      (else
       (cons (meaning (car args) table) (evlis (cdr args) table))
       ))))

(define *application
  (lambda (e table)
    (*apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))
(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))
(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define *apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

(define apply-primitive (lambda (name vals)
(cond
((eq? name 'cons )
(cons (first vals) (second vals))) 
((eq? name (quote car))
(car (first vals)))
((eq? name (quote cdr))
( cdr (first vals))) 
((eq? name (quote null?))
(null? (first vals))) 
((eq? name (quote eq?))
( eq? (first vals) (second vals) )) 
((eq? name (quote atom?))
( :atom? (first vals))) 
((eq? name (quote zero?))
(zero? (first vals)))
((eq? name (quote add1))
(add1 (first vals)))
((eq? name (quote sub1))
(sub1 (first vals)))
((eq? name (quote number?))
(number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure) vals)
              (table-of closure)))))

(*apply (list 'primitive 'cons) (list 6 (list 7 8)))

