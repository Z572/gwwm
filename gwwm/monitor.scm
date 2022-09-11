(define-module (gwwm monitor)
  #:use-module (oop goops)
  #:use-module (wlroots types output)
  #:use-module (wlroots types output-layout)
  #:autoload (system foreign) (pointer-address)
  #:export (current-monitor
            monitor-name
            monitor-description
            monitor-enabled?
            monitor-scale
            monitor-height
            monitor-list
            monitor-width
            monitor-refresh
            monitor-physical-width
            monitor-physical-height
            monitor=?
            monitor-at
            <gwwm-monitor>))

(define-once %monitors
  (make-hash-table))
(define (monitor-list)
  "return all monitors."
  (hash-map->list (lambda (_ b) b) %monitors))
(define (current-monitor)
  "return current monitor or #f not found."
  ((@@ (gwwm) current-monitor)))

(define-class <gwwm-monitor> ()
  (data #:init-keyword #:data #:accessor .data)
  (name #:allocation #:virtual
        #:slot-ref (lambda (m) (wlr-output-name (monitor-wlr-output m)))
        #:slot-set! (lambda _ #f)
        #:getter monitor-name)
  (description #:allocation #:virtual
               #:slot-ref (lambda (m) (wlr-output-description (monitor-wlr-output m)))
               #:slot-set! (lambda _ #f)
               #:getter monitor-description)
  (enabled? #:allocation #:virtual
            #:slot-ref (lambda (m) (wlr-output-enabled (monitor-wlr-output m)))
            #:slot-set! (lambda _ #f)
            #:getter monitor-enabled?)
  (width  #:allocation #:virtual
          #:slot-ref (lambda (m) (wlr-output-width (monitor-wlr-output m)))
          #:slot-set! (lambda _ #f)
          #:getter monitor-width)
  (height #:allocation #:virtual
          #:slot-ref (lambda (m) (wlr-output-height (monitor-wlr-output m)))
          #:slot-set! (lambda _ #f)
          #:getter monitor-height)
  (scale #:allocation #:virtual
         #:slot-ref (lambda (m) (wlr-output-scale (monitor-wlr-output m)))
         #:slot-set! (lambda _ #f)
         #:getter monitor-scale)
  (refresh #:allocation #:virtual
           #:slot-ref (lambda (m) (wlr-output-refresh (monitor-wlr-output m)))
           #:slot-set! (lambda _ #f)
           #:getter monitor-refresh)
  (physical-width #:allocation #:virtual
                  #:slot-ref (lambda (m)
                               (wlr-output-physical-width (monitor-wlr-output m)))
                  #:slot-set! (lambda _ #f)
                  #:getter monitor-physical-width)
  (physical-height #:allocation #:virtual
                   #:slot-ref (lambda (m)
                                (wlr-output-physical-height (monitor-wlr-output m)))
                   #:slot-set! (lambda _ #f)
                   #:getter monitor-physical-height))

(define-method (write (o <gwwm-monitor>) port)
  (format port "#<<gwwm-monitor ~a (~a . ~a) scale: ~a>"
          (monitor-name o)
          (monitor-width o)
          (monitor-height o)
          (monitor-scale o)))

(define (monitor-at x y)
  (and=> (wlr-output-layout-output-at
          ((@@ (gwwm) gwwm-output-layout)) x y)
         (lambda (o)
           (hash-ref %monitors
                     (pointer-address
                      (wlr-output-data o))))))
(define-method (equal? (o1 <gwwm-monitor>)
                       (o2 <gwwm-monitor>))
  (monitor=? o1 o2))

(define-method (monitor=? (o1 <gwwm-monitor>)
                          (o2 <gwwm-monitor>))
  (equal? (.data o1) (.data o2)))

(define (monitor-wlr-output m)
  ((@@ (gwwm) monitor-wlr-output) m))
