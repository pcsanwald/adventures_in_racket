(define add1
  (lambda (n)
    (+ n 1)
   )
)

(define sub1
  (lambda (n)
    (- n 1)
   )
)

(define plus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (plus a (sub1 b))))
    )
  )
)

(define minus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (minus a (sub1 b))))
    )
  )
)

(define addtups
  (lambda (t1 t2)
    (cond
      ((null? t1) (quote()))
      (else
        (cons (plus (car t1) (car t2)) (addtup (cdr t1) (cdr t2)))
       )
    )
  )
)

(define mult
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else
        (plus a (mult a (sub1 b)))
       )
    )
  )
)

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat))))
    )
  )
)

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat)))
    )))

(define rempick
  (lambda (n lat)
    (cond
       ((zero? n) (cdr lat))
       (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))
       )
     )
  )
)