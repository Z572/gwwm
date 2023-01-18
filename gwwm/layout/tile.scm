(define-module (gwwm layout tile)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (gwwm client)
  #:use-module (gwwm layout)
  #:export (tile-layout))
(define (tile m)
  (let ((n (count (lambda (c)
                    (and (visibleon c m)
                         (not (client-floating? c))
                         (not (client-fullscreen? c))))
                  (client-list))))
    (unless (zero? n)
      ((@@ (gwwm) %tile) m n))))
(define tile-layout
  (make <layout>
    #:symbol "[t]"
    #:procedure tile))
