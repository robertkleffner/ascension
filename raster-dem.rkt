#lang racket

(require file/gunzip)
(require racket/class)
(require racket/path)
(require racket/draw)

(provide hgt-port->elevations
         hgt-gz-port->elevations
         hgt-file->elevations
         hgt-gz-file->elevations
         hgt-file->raster-dem
         hgt-gz-file->raster-dem
         raster-dem->bitmap%)



(struct raster-dem (lat-south lon-west lat-step lon-step rows cols elevs) #:transparent)



(define (hgt-port->elevations port)
    (define (rec-hgt-read elevs)
        (define inbytes (read-bytes 2 port))
        (if (eof-object? inbytes)
            elevs
            (rec-hgt-read (cons (integer-bytes->integer inbytes #t #t) elevs))))
    (reverse (rec-hgt-read null)))

(define (hgt-gz-port->elevations port)
    (define-values (hgt-in gz-out) (make-pipe))
    (gunzip-through-ports port gz-out)
    (close-output-port gz-out)
    (define elevs (hgt-port->elevations hgt-in))
    (close-input-port hgt-in)
    elevs)

(define (hgt-file->elevations path)
    (call-with-input-file path hgt-port->elevations #:mode 'binary))

(define (hgt-gz-file->elevations path)
    (call-with-input-file path hgt-gz-port->elevations #:mode 'binary))

(define (get-geo-vals-from-path path rows cols)
    (define filename (path->string (file-name-from-path path)))
    (define pos-lat (string=? "N" (substring filename 0 1)))
    (define pos-lon (string=? "E" (substring filename 3 4)))
    (define rel-lat (string->number (substring filename 1 3)))
    (define rel-lon (string->number (substring filename 4 7)))
    (define lat (if pos-lat rel-lat (- rel-lat)))
    (define lon (if pos-lon rel-lon (- rel-lon)))
    (define lat-step (/ 1 cols))
    (define lon-step (/ 1 rows))
    (values lat lon lat-step lon-step))

(define (hgt-file->raster-dem path rows cols)
    (define-values (lat lon lat-step lon-step) (get-geo-vals-from-path path rows cols))
    (define elevs (hgt-file->elevations path))
    (raster-dem lat lon lat-step lon-step rows cols elevs))

(define (hgt-gz-file->raster-dem path rows cols)
    (define-values (lat lon lat-step lon-step) (get-geo-vals-from-path path rows cols))
    (define elevs (hgt-gz-file->elevations path))
    (raster-dem lat lon lat-step lon-step rows cols elevs))



(define (raster-dem->bitmap% dem)
    (define cols (raster-dem-cols dem))
    (define max-elev (apply max (raster-dem-elevs dem)))
    (define min-elev (apply min (raster-dem-elevs dem)))
    (define norm-factor (/ 1 (- max-elev min-elev)))
    (define target (make-bitmap (raster-dem-rows dem) (raster-dem-cols dem)))
    (define dc (new bitmap-dc% [bitmap target]))
    (for ([(elev index) (in-indexed (raster-dem-elevs dem))])
        (define x (remainder index cols))
        (define y (quotient index cols))
        (define pixel (exact-round (* norm-factor (- elev min-elev) 255)))
        (send dc set-pixel x y (instantiate color% (pixel pixel pixel))))
    target)