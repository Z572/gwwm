(define-module (gwwm monitor)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 q)
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
            monitor-wlr-output
            monitor-layouts
            monitor-window-area
            monitor-area
            monitor-sellt
            <gwwm-monitor>
            %monitors))

(define-once %monitors
  (make-parameter
   (make-q)
   (lambda (o)
     (if (q? o) o
         (error "not a q! ~A" o)))))
(define (monitor-list)
  "return all monitors."
  (car (%monitors)))
(define-once %current-monitor #f)
(define (get-current-monitor)
  %current-monitor)
(define (set-current-monitor m)
  (set! %current-monitor m))
(define current-monitor (make-procedure-with-setter
                         get-current-monitor
                         set-current-monitor))
(define-class <gwwm-monitor> ()
  (data #:init-keyword #:data #:accessor .data #:class <hidden-slot>)
  (name #:allocation #:virtual
        #:slot-ref (lambda (m) (.name (monitor-wlr-output m)))
        #:slot-set! (lambda _ #f)
        #:getter monitor-name)
  (area #:accessor monitor-area
        #:setter set-.area!)
  (window-area #:accessor monitor-window-area
               #:setter set-.window-area!)
  (description #:allocation #:virtual
               #:slot-ref (lambda (m) (.description (monitor-wlr-output m)))
               #:slot-set! (lambda _ #f)
               #:getter monitor-description)
  (enabled? #:allocation #:virtual
            #:slot-ref (lambda (m) (.enabled (monitor-wlr-output m)))
            #:slot-set! (lambda _ #f)
            #:getter monitor-enabled?)
  (width  #:allocation #:virtual
          #:slot-ref (lambda (m) (.width (monitor-wlr-output m)))
          #:slot-set! (lambda _ #f)
          #:getter monitor-width)
  (height #:allocation #:virtual
          #:slot-ref (lambda (m) (.height (monitor-wlr-output m)))
          #:slot-set! (lambda _ #f)
          #:getter monitor-height)
  (scale #:allocation #:virtual
         #:slot-ref (lambda (m) (.scale (monitor-wlr-output m)))
         #:slot-set! (lambda _ #f)
         #:getter monitor-scale)
  (refresh #:allocation #:virtual
           #:slot-ref (lambda (m) (.refresh (monitor-wlr-output m)))
           #:slot-set! (lambda _ #f)
           #:getter monitor-refresh)
  (physical-width #:allocation #:virtual
                  #:slot-ref (lambda (m)
                               (.phys-width (monitor-wlr-output m)))
                  #:slot-set! (lambda _ #f)
                  #:getter monitor-physical-width)
  (physical-height #:allocation #:virtual
                   #:slot-ref (lambda (m)
                                (.phys-height (monitor-wlr-output m)))
                   #:slot-set! (lambda _ #f)
                   #:getter monitor-physical-height)
  (scene-output #:accessor monitor-scene-output)
  (wlr-output #:accessor monitor-wlr-output
              #:setter set-.wlr-output!)
  (layouts #:init-value (list #f #f)
           #:accessor monitor-layouts
           #:setter set-.monitor-layouts)
  (sellt #:init-value 0
         #:accessor monitor-sellt
         #:setter set-.monitor-sellt!))

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
           (let ((b (pointer-address
                     (.data o))))
             (find (lambda (m) (= (.data m) b))
                   (monitor-list))))))
(define-method (equal? (o1 <gwwm-monitor>)
                       (o2 <gwwm-monitor>))
  (monitor=? o1 o2))

(define-method (monitor=? (o1 <gwwm-monitor>)
                          (o2 <gwwm-monitor>))
  (equal? (.data o1) (.data o2)))
