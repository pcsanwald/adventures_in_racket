(define (atom? thing)
     (not (list? thing))
 )   

(define (firsts lat)
    (cond
        ((null? lat) (quote()))
        (else (cond
            ( (atom? (car lat)) (cons (car lat) (firsts (cdr lat))))
            (else (cons (car (car lat)) (firsts (cdr lat))))
        ))  
    )   
)

(define (insertR new old lat)
  (cond
    ((null? lat) (quote()))
    (else (cond
            ( (eq? (car lat) old) (cons (car lat) (cons new (cdr lat)))) 
            (else (cons (car lat) (insertR new old (cdr lat)) )) 
          )
          )
    )
  )
            