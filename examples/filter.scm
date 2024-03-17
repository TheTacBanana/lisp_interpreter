(define (filter l f)
    (if (empty? l)
        ('())
        (if (f (car l))
            (cons (car l) (filter (cdr l) f))
            (filter (cdr l) f))))

(define (even? x)
    (if (eq? x 0)
        #t
        (if (eq? x 1)
            #f
            (even? (- x 2))
        )
    ))

(define (odd? x) (not (even? x)))

(write (filter '(1 2 3 4 5 6) odd?))