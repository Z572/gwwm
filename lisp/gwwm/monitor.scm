(define-module (gwwm monitor)
  #:use-module (wlroots types output)
  #:export (monitor-height
            monitor-width))

(define (monitor-wlr-output m)
  ((@@ (gwwm) monitor-wlr-output) m))

(define (monitor-height m)
  (wlr-output-height (monitor-wlr-output m)))

(define (monitor-width m)
  (wlr-output-width (monitor-wlr-output m)))
