(define (filter l f)
    (if (empty? l)
        '()
        (if (f (car l))
            (cons (car l) (filter (cdr l) f))
            (filter (cdr l) f))))

(define (even? x)
    (if (eq? x 0)
        #t
        (if (eq? x 1)
            #f
            (even? (- x 2))))
    )

(define (nlist n)
    (if (eq? n 0)
        '()
        (cons n (nlist (- n 1)))
    )
)

(write (filter (nlist 100) even?))

; (write (if (even? 10) 1 2))

; (heap-dump)