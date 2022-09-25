(define-module (gwwm layout monocle)
  ;; #:use-module (gwwm )
  #:use-module (ice-9 control)
  #:use-module (oop goops)
  #:use-module (gwwm client)
  #:use-module (gwwm layout)
  #:use-module (gwwm monitor)
  #:use-module (gwwm commands)
  #:export (monocle-layout))
(define (monocle m)
  (let/ec return
    (for-each
     (lambda (c)
       (when (or (not (visibleon c m))
                 (client-floating? c)
                 (client-fullscreen? c))
         (return c))
       (client-resize c (monitor-window-area m)
                      #f))
     (client-list)))
  (focusclient (focustop m) #t))

(define monocle-layout
  (make <layout>
    #:symbol "[m]"
    #:procedure monocle))
