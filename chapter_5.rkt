(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
      ((eq? (car l) a) (rember* a (cdr l)))
      (else (cons (car l) (rember* a (cdr l))))
    )
  )
)

;(rember* 'cup (list 'coffee 'cup 'is 'cup))
;(rember* 'cup (list (list 'coffee) 'cup (list (list 'tea) 'cup) (list 'and (list 'hick)) 'cup))

(define insertr*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((list? (car l)) (cons (insertr* new old (car l)) (insertr* new old (cdr l))))
      ((eq? (car l) old) (cons (cons old new) (insertr* new old (cdr l))))
      (else (cons (car l) (insertr* new old (cdr l))))
    )
))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((list? (car l)) (+ (occur* a (car l)) (occur* a (cdr l))))
      ((eq? (car l) a) (+ 1 (occur* a (cdr l))))
      (else (occur* a (cdr l)))
    )
))

;(insertr* 'roast 'chuck 
 ; (list (list 'how 'much (list 'wood)) 'could (list (list 'a (list 'wood) 'chuck))
  ;      (list (list (list 'chuck)))
   ;     (list 'if (list 'a) (list (list 'wood 'chuck)))
    ;    'could 'chuck 'wood)
;)

#|
(occur* 'chuck 
  (list (list 'how 'much (list 'wood)) 'could (list (list 'a (list 'wood) 'chuck))
        (list (list (list 'chuck)))
        (list 'if (list 'a) (list (list 'wood 'chuck)))
        'could 'chuck 'wood)
)
|#

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((list? (car l)) (cons (subst* new old (car l)) (subst* new old (cdr l))))
      ((eq? (car l) old) (cons new (subst* new old (cdr l))))
      (else (cons (car l) (subst* new old (cdr l))))
    )
))

(define insertl*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((list? (car l)) (cons (insertl* new old (car l)) (insertl* new old (cdr l))))
      ((eq? (car l) old) (cons (cons new old) (insertl* new old (cdr l)) ))
      (else (cons (car l) (insertl* new old (cdr l))))
    )
))

#|
(insertl* 'pecker 'chuck 
  (list (list 'how 'much (list 'wood)) 'could (list (list 'a (list 'wood) 'chuck))
        (list (list (list 'chuck)))
        (list 'if (list 'a) (list (list 'wood 'chuck)))
        'could 'chuck 'wood)
)
|#

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((list? (car l)) (or (member* a (car l)) (member* a (cdr l))))
      ((eq? (car l) a) #t)
      (else (member* a (cdr l)))
    )
))

;(member* 'chips (list (list 'potato) (list 'chips (list (list 'with) 'fish) (list 'chips))))

(define leftmost
  (lambda (l)
    (cond
      ((list? (car l)) (car l))
       (else (leftmost (cdr l)))
       )
      )
    )
;(leftmost (list (list 'potato) (list 'chips (list (list 'with) 'fish) (list 'chips))))

(define (atom? x) (not (list? x)))

(define (equan? a b)
  (cond
    ((and (number? a) (number? b)) (= a b))
    ((or (number? a) (number? b)) #f)
    (else (eq? a b))
  )
)

#|
if both lists are null, return true.
if first items are both atoms, compare the atoms and keep going through the lists
if the first items are both lists, check the equality of the sublists, and keep going through the lists
if the first items are anything but atoms or lists, then they are mismatched and we can return false
|#
(define eqlist?
  (lambda (a b)
    (cond
      ((and (null? a) (null? b)) #t)
      ((and (list? (car a)) (list? (car b))) (and (equlist? (car a) (car b)) (eqlist? (cdr a) (cdr b)) ))
      ((and (atom? (car a)) (atom? (car b))) (and (equan? (car a) (car b)) (eqlist? (cdr a) (cdr b)) ))
      (else #f)
  )
)   
)

(define equal
   (lambda a b
      (cond 
        ((and (atom? a) (atom? b)) (equan? a b))
        ((or (atom? a) (atom? b)) #f)
        (else (eqlist? a b))
       )
   )
)