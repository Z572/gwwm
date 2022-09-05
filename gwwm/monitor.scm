(define-module (gwwm monitor)
  #:use-module (oop goops)
  #:use-module (wlroots types output)
  #:export (current-monitor
            monitor-name
            monitor-description
            monitor-enabled
            monitor-scale
            monitor-height
            monitor-width
            monitor-refresh
            monitor-physical-width
            monitor-physical-height
            monitor=?
            <gwwm-monitor>))

(define (current-monitor)
  "return current monitor or #f not found."
  ((@@ (gwwm) current-monitor)))
(define-class <gwwm-monitor> ()
  (data #:init-keyword #:data #:accessor .data))

(define-method (write (o <gwwm-monitor>) port)
  (format port "#<<gwwm-monitor ~a (~a . ~a) scale: ~a>"
          (monitor-name o)
          (monitor-width o)
          (monitor-height o)
          (monitor-scale o)))

(define-method (equal? (o1 <gwwm-monitor>)
                       (o2 <gwwm-monitor>))
  (monitor=? o1 o2))

(define-method (monitor=? (o1 <gwwm-monitor>)
                          (o2 <gwwm-monitor>))
  (equal? (.data o1) (.data o2)))

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
