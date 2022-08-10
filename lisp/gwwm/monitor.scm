(define-module (gwwm monitor)
  #:use-module (wlroots types output)
  #:export (monitor-name
            monitor-description
            monitor-enabled
            monitor-scale
            monitor-height
            monitor-width
            monitor-refresh
            monitor-physical-width
            monitor-physical-height))

(define (monitor-wlr-output m)
  ((@@ (gwwm) monitor-wlr-output) m))

(define (monitor-height m)
  (wlr-output-height (monitor-wlr-output m)))

(define (monitor-width m)
  (wlr-output-width (monitor-wlr-output m)))

(define (monitor-name m)
  (wlr-output-name (monitor-wlr-output m)))

(define (monitor-description m)
  (wlr-output-description (monitor-wlr-output m)))

(define (monitor-enabled m)
  (wlr-output-enabled (monitor-wlr-output m)))

(define (monitor-scale m)
  (wlr-output-scale (monitor-wlr-output m)))

(define (monitor-refresh m)
  (wlr-output-refresh (monitor-wlr-output m)))

(define (monitor-physical-width m)
  (wlr-output-physical-width (monitor-wlr-output m)))

(define (monitor-physical-height m)
  (wlr-output-physical-height (monitor-wlr-output m)))
