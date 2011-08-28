; exercises from "truly understanding the Y-combinator

; this is a reference definition for factorial
(define factorial
  (lambda (n)
    (cond 
      ((= n 0) 1)
      (else (* n (factorial (- n 1))))
      )
    ))

; this takes a factorial function, and returns another function that computes a factorial.
; the previous line, on the surface, doesn't make a lot of sense, since if we are passing
; a function that already computes a factorial, then what purpose does almost-factorial serve?
;
(define almost-factorial
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))
 
;(almost-factorial 3)

; stepping stones to Y-combinator
(define identity_ex (lambda (x) x))
(define factorial0 (almost-factorial identity_ex))

; so this call
(factorial0 0)
; is the same as
((almost-factorial (lambda (x) x)) 0)
;which is also the same as this (expanded form)
(((lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))) 
  (lambda (x) x)) 0)

; we can define another function which works for 1
(define factorial1
  (almost-factorial factorial0))
(factorial1 1)
; expanding out, we already expanded factorial 0 above, so...
((almost-factorial factorial0) 1)
; expand out factorial0...
((almost-factorial (almost-factorial (lambda (x) x))) 1)

; one expansion into lambdas gives...
((almost-factorial ((lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))) (lambda (x) x))) 1)
; one more expansion!
(((lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))) ((lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))) (lambda (x) x))) 1)

; expanding....
;(define factorial2
;  (almost-factorial factorial1))
;(factorial2 2)

;(define factorial3
;  (almost-factorial factorial3))
;(factorial3 3)

; ok, now we realize that what we really want is an infinite chain of these almost-factorial functions,
; so that we can deal with any number. we are going to use a fixpoint function to obtain this. here's why:
; y = (fixpoint y), by definition
; so, if we use substitution here, we realize that a fixpoint function will actually give us the
; infinite stack of functions we are looking for, by substituting the right hand side of the equation
; for the "y" in the right hand equation. in other words:
; y = (fixpoint y)
; y = (fixpoint (fixpoint y))
; y = (fixpoint (fixpoint (fixpoint y)))
; etc...

(define Y
  (lambda (f)
    (f (Y f))))
; note this is not a combinator. but, using lazy racket, we can now define the following:
(define almost-factorial-lazy
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))

; huzzah! we have now made it such that Y is the only function in our language that
; requires explicit recursion. everything else can be defined recursively using Y.
; this is huge.

(define factorial-lazy (Y almost-factorial-lazy))
;(factorial-lazy 4)

; Derive the lazy (normal order) Y combinator
'"derive"
(define (part-factorial self)
  (let ((f (self self)))
  (lambda (n)
  (if (= n 0)
      1
      (* n (f (- n 1)))))))

; expand the "let" statement to a lambda using this handy rule:
; (let ((x s1)) s2) == ((lambda(x) s1) s2) 
(define (part-factorial self)
  ((lambda (f)
     (lambda (n)
       (if (= n 0)
           1
           (* n (f (- n 1))))))
   (self self)))

(define fact-y (part-factorial part-factorial))

(fact-y 5)

(define part-factorial
  (lambda (self)
    (almost-factorial
     (self self))))

(define almost-factorial
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))
(define factorial 
  (let ((x (lambda (self)
             (almost-factorial (self self)))))
    (x x)))

; once again use our let => lambda trick

(define factorial 
  ((lambda (x) (x x)) 
    (lambda (x)
      (almost-factorial (x x)))))

(factorial 6)

(define (make-recursive f)
  ((lambda (x) (x x))
   (lambda (x) (f (x x)))))

(define factorial (make-recursive almost-factorial))
(factorial 5)

