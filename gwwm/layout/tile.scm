(define-module (gwwm layout tile)
  #:use-module (oop goops)
  #:use-module (gwwm layout)
  #:export (tile-layout))
(define (tile m)
  ((@@ (gwwm) %tile) m))
(define tile-layout
  (make <layout>
    #:symbol "[t]"
    #:procedure tile))
