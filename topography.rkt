#lang racket/base

(require "raster-dem.rkt")
(require "util.rkt")
(require racket/math)
(require racket/sequence)
(require racket/string)
(require racket/list)
(require racket/set)

(module+ test
    (require rackunit))



(provide only-plateaus
         dem-only-plateaus
         quick-peaks
         dem-quick-peaks
         quick-pits
         dem-quick-pits
         quick-saddles
         dem-quick-saddles
         critical-plateaus
         dem-critical-plateaus
         critical-points
         dem-critical-points
         vector-flood-fill!
         critical-point
         critical-point-type
         critical-point-index
         critical-point-elev
         critical-point-shape)



(struct critical-point (type index elev shape) #:transparent)



(define (neighbors? ind1 ind2 row-width)
    (define-values (x1 y1) (index->xy-values ind1 row-width))
    (define-values (x2 y2) (index->xy-values ind2 row-width))
    (and (> 2 (abs (- x1 x2)))
         (> 2 (abs (- y1 y2)))))

(define (neighbor-of-any? index inds row-width)
    (for/or ([i inds])
        (neighbors? i index row-width)))

; | 0 | 1 | 2 | 3 |
; | 4 | 5 | 6 | 7 |
; | 8 | 9 | 10 | 11 |
(module+ test
    (check-true (neighbors? 0 1 4))
    (check-true (neighbors? 0 4 4))
    (check-true (neighbors? 0 5 4))
    (check-false (neighbors? 0 2 4))
    (check-false (neighbors? 0 3 4)))



(define (all-neighbors? ind width height fn)
    (define-values (x y) (index->xy-values ind width))
    (define x-min (max 0 (sub1 x)))
    (define x-max (min (sub1 width) (add1 x)))
    (define y-min (max 0 (sub1 y)))
    (define y-max (min (sub1 height) (add1 y)))
    (for*/and ([xi (in-range x-min (add1 x-max))]
               [yi (in-range y-min (add1 y-max))]
               #:when (not (= ind (xy-values->index xi yi width))))
        (fn (xy-values->index xi yi width))))

(define (any-neighbors? ind width height fn)
    (define-values (x y) (index->xy-values ind width))
    (define x-min (max 0 (sub1 x)))
    (define x-max (min (sub1 width) (add1 x)))
    (define y-min (max 0 (sub1 y)))
    (define y-max (min (sub1 height) (add1 y)))
    (for*/or ([xi (in-range x-min (add1 x-max))]
              [yi (in-range y-min (add1 y-max))]
              #:when (not (= ind (xy-values->index xi yi width))))
        (fn (xy-values->index xi yi width))))

(define (all-neighbors-lower? elevs elev ind width height)
    (define (higher? i) (> elev (vector-ref elevs i)))
    (all-neighbors? ind width height higher?))

(define (all-neighbors-higher? elevs elev ind width height)
    (define (lower? i) (< elev (vector-ref elevs i)))
    (all-neighbors? ind width height lower?))

(define (any-equal-neighbor? elevs elev ind width height)
    (define (same-elev? i) (= elev (vector-ref elevs i)))
    (any-neighbors? ind width height same-elev?))

(define (neighbor-indices-clockwise ind width height)
    (define-values (x y) (index->xy-values ind width))
    (define-values (x1 x-1) (values (add1 x) (sub1 x)))
    (define-values (y1 y-1) (values (add1 y) (sub1 y)))
    (define neighbors (list (cons x-1 y-1) (cons x y-1) (cons x1 y-1) (cons x1 y)
                            (cons x1 y1) (cons x y1) (cons x-1 y1) (cons x-1 y)))
    (for/list ([n neighbors]
               #:when (and (> (car n) 0) (> (cdr n) 0) (< (car n) width) (< (cdr n) height)))
        (xy-values->index (car n) (cdr n) width)))

(define (vector-flood-fill! vec-to-fill width height fill-val start-ind fill-pred)
    (define map-size (vector-length vec-to-fill))
    (define (flood-fill-rec ind)
        (when (and (not (equal? (vector-ref vec-to-fill ind) fill-val))
                   (fill-pred ind))
            (vector-set! vec-to-fill ind fill-val)
            (when (> ind 0)
                (flood-fill-rec (sub1 ind)))
            (when (< ind (sub1 map-size))
                (flood-fill-rec (add1 ind)))
            (define cell-north (- ind width))
            (define cell-south (+ ind width))
            (when (>= cell-north 0)
                (define i-min (max 0 (sub1 cell-north)))
                (define i-max (add1 cell-north))
                (for ([i (in-range i-min i-max)])
                    (flood-fill-rec i)))
            (when (< cell-south map-size)
                (define i-min (sub1 cell-south))
                (define i-max (min map-size (add1 cell-south)))
                (for ([i (in-range i-min i-max)])
                    (flood-fill-rec i)))))
    (flood-fill-rec start-ind))

(define (dem-critical-points dem)
    (critical-points (raster-dem-cols dem)
                     (raster-dem-rows dem)
                     (raster-dem-elevs dem)))

(define (critical-points width height elevs)
    (define plateaus (only-plateaus width height elevs))
    
    (define peaks (quick-peaks width height elevs))
    (define pits (quick-pits width height elevs))
    (define saddles (quick-saddles width height elevs))
    
    (define-values (plat-peaks plat-pits plat-saddles filtered-slopes)
        (critical-plateaus width height plateaus elevs))
    
    (values (append peaks plat-peaks) (append pits plat-pits) (append saddles plat-saddles) filtered-slopes))

(define (dem-critical-plateaus dem-plateaus dem-full)
    (critical-plateaus (raster-dem-cols dem-full)
                       (raster-dem-rows dem-full)
                       (raster-dem-elevs dem-plateaus)
                       (raster-dem-elevs dem-full)))

(define (critical-plateaus width height plateau-elevs full-elevs)
    (define domain-filled (add1 NO-DATA))
    (define domains (for/vector ([e (in-vector plateau-elevs)]) NO-DATA))
    (define map-size (vector-length plateau-elevs))

    (define (get-domain-boundary index elev body boundary)
        (define same-elev? (= (vector-ref plateau-elevs index) elev))
        (when (not same-elev?) (set-add! boundary index))
        (when (and same-elev? (= (vector-ref domains index) NO-DATA))
            (vector-set! domains index domain-filled)
            (set-add! body index)
            (when (> index 0)
                (get-domain-boundary (sub1 index) elev body boundary))
            (when (< index (sub1 map-size))
                (get-domain-boundary (add1 index) elev body boundary))
            (define cell-north (- index width))
            (define cell-south (+ index width))
            (when (>= cell-north 0)
                (define i-min (max 0 (sub1 cell-north)))
                (define i-max (add1 cell-north))
                (for ([i (in-range i-min i-max)])
                    (get-domain-boundary i elev body boundary)))
            (when (< cell-south map-size)
                (define i-min (sub1 cell-south))
                (define i-max (min map-size (add1 cell-south)))
                (for ([i (in-range i-min i-max)])
                    (get-domain-boundary i elev body boundary)))))
    
    (define (extract-segment pixels)
        (for/fold ([segment (list (first pixels))] [remaining null])
                  ([p (rest pixels)])
            (cond
                [(neighbor-of-any? p segment width)
                 (values (cons p segment) remaining)]
                [else
                 (values segment (cons p remaining))])))

    (define (segmentize pixels)
        (cond
            [(null? pixels) null]
            [else
             (define-values (segment remaining) (extract-segment pixels))
             (cons segment (segmentize remaining))]))
    
    (define (get-boundary-segments boundary elev)
        (define-values (highers lowers)
            (for/fold ([higher null] [lower null])
                      ([p (in-set boundary)])
                (if (< (vector-ref full-elevs p) elev)
                    (values higher (cons p lower))
                    (values (cons p higher) lower))))
        (values (segmentize highers) (segmentize lowers)))
    
    (define (categorize-plateau highers lowers)
        (cond
            [(null? highers) 'peak]
            [(null? lowers) 'pit]
            [(and (null? (rest highers)) (null? (rest lowers))) 'slope]
            [else 'saddle]))

    (for/fold ([peaks null] [pits null] [saddles null] [filtered-slopes 0])
              ([(elev index) (in-indexed (in-vector plateau-elevs))]
               #:when (and (not (= elev NO-DATA)) (= (vector-ref domains index) NO-DATA)))
        (define boundary (mutable-set))
        (define body (mutable-set))
        (get-domain-boundary index elev body boundary)
        (define-values (higher-segments lower-segments) (get-boundary-segments boundary elev))
        (case (categorize-plateau higher-segments lower-segments)
            [(peak) (values (cons (critical-point 'peak index elev (set->list body)) peaks) pits saddles filtered-slopes)]
            [(pit) (values peaks (cons (critical-point 'pit index elev (set->list body)) pits) saddles filtered-slopes)]
            [(saddle) (values peaks pits (cons (critical-point 'saddle index elev (cons higher-segments lower-segments)) saddles) filtered-slopes)]
            [else (values peaks pits saddles (add1 filtered-slopes))])))

(define (dem-only-plateaus dem)
    (define plateaus (only-plateaus (raster-dem-cols dem) (raster-dem-rows dem) (raster-dem-elevs dem)))
    (same-dimension-raster-dem dem plateaus))

(define (only-plateaus width height elevs)
    (define els (if (list? elevs) (list->vector elevs) elevs))
    (define (local-equal-neighbor? elev ind)
        (any-equal-neighbor? els elev ind width height))
    (for/vector ([(elev index) (in-indexed (in-vector els))])
        (if (local-equal-neighbor? elev index) elev NO-DATA)))

(define (dem-quick-peaks dem)
    (quick-peaks (raster-dem-cols dem)
                 (raster-dem-rows dem)
                 (raster-dem-elevs dem)))

(define (quick-peaks width height elevs)
    (define els (if (list? elevs) (list->vector elevs) elevs))
    (define (local-neighbors-lower? elev ind)
        (all-neighbors-lower? els elev ind width height))
    (for/list ([(elev index) (in-indexed (in-vector els))]
               #:when (local-neighbors-lower? elev index))
        (critical-point 'peak index elev null)))

(define (dem-quick-pits dem)
    (quick-pits (raster-dem-cols dem)
                (raster-dem-rows dem)
                (raster-dem-elevs dem)))

(define (quick-pits width height elevs)
    (define els (if (list? elevs) (list->vector elevs) elevs))
    (define (local-neighbors-higher? elev ind)
        (all-neighbors-higher? els elev ind width height))
    (for/list ([(elev index) (in-indexed (in-vector els))]
               #:when (local-neighbors-higher? elev index))
        (critical-point 'pit index elev null)))

(define (dem-quick-saddles dem)
    (quick-saddles (raster-dem-cols dem)
                   (raster-dem-rows dem)
                   (raster-dem-elevs dem)))

(define (quick-saddles width height elevs)
    (define els (if (list? elevs) (list->vector elevs) elevs))
    (define (local-equal-neighbor? elev ind)
        (any-equal-neighbor? els elev ind width height))
    (define (more-than-two-sign-changes? elev ind)
        (define signs
            (for/list ([neighbor (neighbor-indices-clockwise ind width height)])
                (sgn (- elev (vector-ref els neighbor)))))
        (> (length (compress-consecutive-duplicates signs)) 3))
    (for/list ([(elev index) (in-indexed (in-vector els))]
               #:when (and (not (local-equal-neighbor? elev index))
                           (more-than-two-sign-changes? elev index)))
        (critical-point 'saddle index elev (neighbor-indices-clockwise index width height))))