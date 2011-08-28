(define (atom? x) (not (list? x)))

(define numbered?
  (lambda (e)
    (cond
      ((atom? e) (number? e) )
      ((equal? (car (cdr e)) "+") (and (numbered? (car e)) (numbered? (car (cdr (cdr e))))) )
      ((equal? (car (cdr e)) "*") (and (numbered? (car e)) (numbered? (car (cdr (cdr e))))) )
      ((equal? (car (cdr e)) "^") (and (numbered? (car e)) (numbered? (car (cdr (cdr e))))) )
      (else #f)
    )
   )
)

(define value
  (lambda (e)
    (cond
      ((atom? e) e)
      ((null? (cdr e)) (value (car e)))
      ((equal? (car (cdr e)) "+") (+ (value (car e)) (value (cdr (cdr e)))) )
      ((equal? (car (cdr e)) "*") (* (value (car e)) (value (cdr (cdr e)))) )
      ((equal? (car (cdr e)) "-") (- (value (car e)) (value (cdr (cdr e)))) )
)))