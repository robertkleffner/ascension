#lang info
(define version 1.0)
(define pkg-authors '(robkleffner))
(define collection "ascension")
(define deps '("base"
               "rackunit-lib"
               "draw-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/ascension.scrbl" ())))
(define pkg-desc "Tools and algorithms for processing digital elevation data.")
