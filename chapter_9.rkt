(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))
    ))
(define third
  (lambda (p)
    (car (cdr (cdr p)))
    ))
(define build
  (lambda (a b)
    (cons a (cons b (quote())))
    ))
(define a-pair?
  (lambda (a)
    (cond
      ((atom? a) #f)
      ((null? (cdr a)) #f)
      ((null? (cdr a)) #f)
      (not (null? (cdr (cdr a))))
      (else #f)
      )))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a i lat)
    (cond
      ((number? i) (keep-looking a (pick i lat) lat))
      (else (eq? i a))
      )))
      
(define atom?
  (lambda (a)
  (not (list? a))))

(define shift
  (lambda (pair)
    (build (first (first pair)) (build (second (first pair)) (second pair)))
    ))
;(shift (list (list 1 2) (list 3 4)))

(define align
  (lambda (a)
    (cond
      ((atom? a) a)
      ((a-pair? (first a))
       (align (shift a)))
       (else (build (first a) (align (second a))))
       )))

(define length*
  (lambda (a)
    (cond
    ( (atom? (car a)) 1)
    (else (+ (length* (car a)) (length* (cdr a))))
    )))

(define weight*
  (lambda (a)
    (cond
      ((atom? a) 1)
      (else
       (+ (* (weight* (first a)) 2)
             (weight* (second a)))))))
(weight* (list (list 1 2) 3))
