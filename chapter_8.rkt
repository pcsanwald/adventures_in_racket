(define rember-f
  (lambda (test? a lat)
    (cond
      ((null? lat) (quote ()))
      ((test? a (car lat)) (rember-f test? a (cdr lat)))
      (else (cons (car lat) (rember-f test? a (cdr lat)) ))
      )))
;(rember-f eq? 'a (list 'b 'c 'a 'd 'a))

(define eq?-c
(lambda (a)
  (lambda (x)
    (eq? x a)))
)

(define eq?-salad
  (eq?-c 'salad))

;(eq?-salad 'salad)

;((eq?-c 'x) 'y)

(define rember-c
  (lambda (t)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((t a (car lat)) ((rember-c t) a (cdr lat)))
        (else (cons (car lat) ((rember-c t) a (cdr lat)) ))
        ))))

(define rember-eq? (rember-c eq?))
;(rember-eq? 't (list 't 's 'i 'g))
;((rember-c eq?) 't (list 'sh 'sa 'a 'sa))

(define insertL
  (lambda (t)
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((t (car lat) old) (cons new (cons old (cdr lat))))
    (else (cons (car lat) ((insertL t) new old (cdr lat)) ))
    )))
  )

(define insertR
  (lambda (t)
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((t (car lat) old) (cons old (cons new (cdr lat))))
    (else (cons (car lat) ((insertR t) new old (cdr lat)) ))
    )))
  )
;((insertR eq?)'a 'b (list 'b 'b 'b))

(define insert-g
  (lambda (t b)
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((t (car lat) old) (b new old lat) )
    (else (cons (car lat) ((insertR t) new old (cdr lat)) ))
    )))
  )

;(
; (insert-g eq? (lambda (new old lat)(cons old (cons new (cdr lat))))) 'a 'c (list 'c 'c 'c)
;)

(define subst
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      (( eq? (car l) old) (cons new (cdr l)))
      (else (cons (car l) (subst new old (cdr l))))
      )))

(define seqrem
  (lambda (new old l) l))

(define yyy
  (lambda (a l)    
    ((insert-g eq? seqrem) #f a l)))

;(yyy 'sa (list 'p 'w 'sa 'a 'b))



(define atom-to-function
  (lambda (x)
    (cond
      ((eq? '+ x) +)
      ((eq? 'x x) *)
      (else ^))))

(define value
  (lambda (e)
    (cond 
      ((atom? e) e)
      ((null? (cdr e)) (value (car e)))
      (else ((atom-to-function (car (cdr e))) (value (car e)) (value (cdr (cdr e)))) )
)))

(define multirember-f
  (lambda (test?)
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
         (else (cons (car lat) ((multirember-f test?) a (cdr lat))))
         )
       )
      ))))

(define multiremberT
  (lambda (cmp? lat)
    (cond
      ((null? lat) (quote()))
      ((cmp? (car lat)) (multiremberT cmp? (cdr lat)))
      (else (cons (car lat) (multiremberT cmp? (cdr lat)))) 
      )))

;(multiremberT (eq?-c 'b) (list 'a 'b 'a 'c))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col (quote()) (quote())))
      ((eq? (car lat) a) (multirember&co a (cdr lat) (lambda (newlat seen) (col newlat (cons (car lat) seen)))))
      (else multirember&co a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen))))))

(define a-friend
  (lambda (x y)
    (null? y)))

;(multirember&co 't (list 's 't 'a 'w) a-friend)

;(multirember&co 't (quote()) a-friend)

;(multirember&co 't (list 't) a-friend)

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons (car lat) seen))))

(define multiinsertL (lambda (new old lat)
(cond ((null? lat) (quote ())) ((eq? (car lat) old)
(cons new (cons old
(multiinsertL new old (cdr lat)))))
(else (cons (car lat) (multiinsertL new old
(cdr lat)))))))

(define multiinsertR (lambda (new old lat)
(cond ((null? lat) (quote ())) ((eq? (car lat) old)
(cons old (cons new
(multiinsertR new old (cdr lat)))))
(else (cons (car lat) (multiinsertR new old
(cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond 
      ((null? lat) (quote()))
      ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))
      )))

(define multiinsertLR&co
  (lambda (new oldL oldR lat k)
    (cond 
      ((null? lat) (quote()) (k (quote()) 0 0))
      ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR&co new oldL oldR (cdr lat)
                                                                   (lambda(newlat L R)
                                                                     (k (cons new (cons oldL newlat)) (+ 1 L) R)
                                                                     )
                                                                   ))))
      ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR&co new oldL oldR (cdr lat) 
                                                                   (lambda(newlat L R)
                                                                     (k (cons oldR (cons new newlat)) L (+ 1 R))
                                                                     )
                                                                   ))))
      (else (multiinsertLR&co new oldL oldR (cdr lat) 
                                              (lambda(newlat L R)
                                                (k (cons (car lat) newlat) L R)
                                              )
                                              ))
      )))


(define ev?
  (lambda (n)
    (= (modulo n 2) 0)))

(define atom?
  (lambda (a)
    (not (list? a))
    ))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ( (ev? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))
         ))
       (else (cons (evens-only* (car l)) (evens-only* (cdr l))))
      )))

(evens-only* (list (list 9 1 2 8) 3 10 (list (list 9 9 ) 7 6) 2))

(define evens-only*&co
  (lambda (l k)
    (cond
      ((null? l) (k (quote()) 1 0 ))
      ((atom? (car l))
       (cond
         ( (ev? (car l)) (evens-only*&co (cdr l) 
                            (lambda (n e o)
                                (k (cons (car l) n) (* (car l) e) o))
                          )
            )
         (else 
          (evens-only*&co (cdr l)
                          (lambda (n e o)
                            (k n e (+ (car l) o))
                          )
          )
         )))
       (else (cons 
              (evens-only*&co (car l) k) 
              (evens-only*&co (cdr l) k)
              ))
      )))
(define the-last-friend
  (lambda (new1 product sum)
    (cons sum (cons product new1))
    ))

(evens-only*&co (list (list 9 1 2 8) 3 10 (list (list 9 9) 7 6) 2) the-last-friend)
