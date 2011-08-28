(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((list? (car l)) (or (member* a (car l)) (member* a (cdr l))))
      ((eq? (car l) a) #t)
      (else (member* a (cdr l)))
    )
))

(define atom?
  (lambda (a)
  (not (list? a))
  ))

#|
This was my original implementation, which used two functions.
I used two functions because I didn't realize that once I asked
member* on the car of the list, I could throw that element away
completely as it was unique. I should have realized this, as I never
do anything with the contents of b.
There's no use saving things as state if they're never referenced.
|#
(define is-set?
  (lambda (a b)
    (cond
      ((null? a) #t)
      ((member* (car a) b) #f)
      (else (is-set? (cdr a) (cons (car a) b)))
      )))

(define set?
  (lambda (a)
    (is-set? a (quote()))
    ))

;(set? (list 'a 'b 'a))

(define simple-is-set?
  (lambda(a)
    (cond
      ((null? a) #t)
      ((member* (car a) (cdr a)) #f)
      (else (simple-is-set? (cdr a)))
      )))
;(simple-is-set? (list 'a 'b 'a))

(define makeset
  (lambda (a)
    (cond
      ((null? a) (quote()))
      ((member* (car a) (cdr a)) (makeset (cdr a)))
      (else (cons (car a) (makeset (cdr a))))
      )))
;(makeset (list 'a 'b 'a 'c 'a 'd))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) a) (multirember a (cdr lat)))
         (else (cons (car lat) (multirember a (cdr lat))))
         )
       )
      )))

;(multirember 'a (list 'a 'b 'a 'c))

(define makeset2
  (lambda (a)
    (cond
      ((null? a) (quote()))
      ((member* (car a) (cdr a)) (makeset2 (multirember (car a) (cdr a))))
      (else (cons (car a) (makeset2 (cdr a))))
      )))

; (makeset2 (list 'a 'b 'a 'c 'a 'd))

(define subset?
  (lambda (a b)
    (cond
      ((null? a) #t)
      ((member* (car a) b) (subset? (cdr a) b))
      (else #f)
      )))

(define subset2?
  (lambda (a b)
    (cond
      ((null? a) #t)
      (else (and (member* (car a) b) (subset? (cdr a) b)))
      )))


;(subset2? (list 'a 'b) (list 'b 'c 'd 'a 'e))
;(subset2? (list 'a 'b 'c) (list 'b 'c))

(define eqset-naive?
  (lambda (a b)
    (cond
      ((null? a) #t)
      ((member* (car a) b) (eqset? (cdr a) b))
      ((or (not (is-set? a)) (not (is-set? b))) #f)
      (else #f)
      )))

(define eqset?
  (lambda (a b)
    (and (subset? a b) (subset? b a))
    ))
;(eqset? (list 'a 'b 'c) (list 'b 'c 'a))
 
(define intersect?
  (lambda (a b)
    (cond
       ((null? a) #f)
       ((member* (car a) b) #t)
       (else (intersect? (cdr a) b))
       )))

(define intersect2?
  (lambda (a b)
    (cond
       ((null? a) #f)
       ((or (member* (car a) b) #t) (intersect? (cdr a) b))
       )))
;(intersect2? (list 'a 'b 'c 'd) (list 'c 'd))

(define intersect
  (lambda (a b)
    (cond
      ((null? a) (quote ()))
      ((member* (car a ) b) (cons (car a) (intersect (cdr a) b)))
      (else (intersect (cdr a) b))
      )))
;(intersect (list 's 't 'a 'm) (list 'm 'a 'c))

; I originally wrote union this way, it's kinda wrong
(define wrong_union
  (lambda (a b)
    (cond
      ((null? a) (quote ()))
      ((member* (car a) (intersect a b)) (cons (car a) (union (cdr a) (multirember (car a) b))))
      (else (cons (car a) (union (cdr a) b) )) 
       )))
; better way to do it is take elements off a and put them onto b, if they are not already
; members of b. if they are in b already, then just call discard the car of a and continue
; recursing
(define union
  (lambda (a b)
    (cond
      ((null? a) b)
      ((member* (car a) b) (union (cdr a) b))
      (else (cons (car a) (union (cdr a) b))) 
       )))
;(union (list 's 't 'a 'm 'c) (list 'm 'a 'c))

(define intersect-all
  (lambda (a)
    (cond
      ((null? (cdr a)) (car a))
      (else (intersect (car a) (intersect-all (cdr a))) )
       )))
;(intersect-all (list (list 6 'p 'a) (list 3 'p 'a 6 'p) (list 8 'p 'a 6 'p) (list 'a 6))) 

(define a-pair?
  (lambda (a)
    (cond
      ((atom? a) #f)
      ((null? (cdr a)) #f)
      ((null? (cdr a)) #f)
      (not (null? (cdr (cdr a))))
      (else #f)
      )))

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
(define (firsts lat)
    (cond
        ((null? lat) (quote()))
        (else (cond
            ( (atom? (car lat)) (cons (car lat) (firsts (cdr lat))))
            (else (cons (car (car lat)) (firsts (cdr lat))))
        ))
    )
)

(define fun?
  (lambda (a)
    (set? (firsts a))
    ))
;(fun? (list (list 'd 4) (list 'b 0) (list 'b 9)))

(define revrel
  (lambda (rel)
    (cond 
      ((null? rel) (quote()))
    (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel))))
    )))
;(revrel (list (list 1 2) (list 3 4) (list 5 6)))

(define seconds
  (lambda (rel)
    (cond
        ((null? rel) (quote()))
        (else (cons (second (car rel)) (seconds (cdr rel))))
        )))
;(seconds (list (list 1 2) (list 3 4) (list 5 6)))

(define fullfun?
  (lambda (rel)
    (and (set? (firsts rel)) (set? (seconds rel)))
    ))
(fullfun? (list (list 'g 'r) (list 'pl 'pr) (list 's 'pr)))