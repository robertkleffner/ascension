#lang racket/base

(require racket/class)
(require racket/draw)



(provide mark-pixels
         index->xy-values
         xy-values->index
         compress-consecutive-duplicates
         vector-min
         vector-max)



(define (mark-pixels target indices color)
    (define dc (new bitmap-dc% [bitmap target]))
    (for ([ind indices])
        (define-values (x y) (index->xy-values ind (send target get-width)))
        (send dc set-pixel x y color))
    target)

(define (index->xy-values ind row-width)
    (values (remainder ind row-width) (quotient ind row-width)))

(define (xy-values->index x y row-width)
    (+ x (* row-width y)))

(define (compress-consecutive-duplicates ls)
    (define-values (reduced a1)
        (for/fold ([acc null]
                   [last #f])
                  ([l (in-list ls)])
            (cond
                [(equal? l last) (values acc last)]
                [else (values (cons l acc) l)])))
    reduced)

(define (vector-min v)
    (for/fold ([m +inf.0]) ([x (in-vector v)])
        (min m x)))

(define (vector-max v)
    (for/fold ([m -inf.0]) ([x (in-vector v)])
        (max m x)))