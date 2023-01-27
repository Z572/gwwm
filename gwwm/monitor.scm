(define-module (gwwm monitor)
  #:autoload (gwwm) (gwwm-output-layout)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 q)
  #:use-module (oop goops)
  #:use-module (util572 box)
  #:use-module (wayland list)
  #:use-module (wlroots types)
  #:use-module (wlroots types output)
  #:use-module (wlroots types output-layout)
  #:use-module (bytestructure-class)
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
            monitor-at
            monitor-output
            monitor-layouts
            monitor-window-area
            monitor-scene-output
            monitor-area
            monitor-sellt
            monitor-nmaster
            monitor-mfact
            dirtomon
            <gwwm-monitor>
            %monitors
            wlr-output->monitor))

(define-once wlr-output->monitor (make-object-property))
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
  (name #:allocation #:virtual
        #:slot-ref (lambda (m) (.name (monitor-output m)))
        #:slot-set! (lambda _ #f)
        #:getter monitor-name)
  (area #:accessor monitor-area
        #:setter set-.area!)
  (window-area #:accessor monitor-window-area
               #:setter set-.window-area!)
  (description #:allocation #:virtual
               #:slot-ref (lambda (m) (.description (monitor-output m)))
               #:slot-set! (lambda _ #f)
               #:getter monitor-description)
  (enabled? #:allocation #:virtual
            #:slot-ref (lambda (m) (.enabled (monitor-output m)))
            #:slot-set! (lambda _ #f)
            #:getter monitor-enabled?)
  (width  #:allocation #:virtual
          #:slot-ref (lambda (m) (.width (monitor-output m)))
          #:slot-set! (lambda _ #f)
          #:getter monitor-width)
  (height #:allocation #:virtual
          #:slot-ref (lambda (m) (.height (monitor-output m)))
          #:slot-set! (lambda _ #f)
          #:getter monitor-height)
  (scale #:allocation #:virtual
         #:slot-ref (lambda (m) (.scale (monitor-output m)))
         #:slot-set! (lambda _ #f)
         #:getter monitor-scale)
  (refresh #:allocation #:virtual
           #:slot-ref (lambda (m) (.refresh (monitor-output m)))
           #:slot-set! (lambda _ #f)
           #:getter monitor-refresh)
  (physical-width #:allocation #:virtual
                  #:slot-ref (lambda (m)
                               (.phys-width (monitor-output m)))
                  #:slot-set! (lambda _ #f)
                  #:getter monitor-physical-width)
  (physical-height #:allocation #:virtual
                   #:slot-ref (lambda (m)
                                (.phys-height (monitor-output m)))
                   #:slot-set! (lambda _ #f)
                   #:getter monitor-physical-height)
  (scene-output #:accessor monitor-scene-output)
  (wlr-output #:accessor monitor-output
              #:setter set-.wlr-output!
              #:init-keyword #:wlr-output)
  (layouts #:init-value (list #f #f)
           #:accessor monitor-layouts
           #:setter set-.monitor-layouts
           #:init-keyword #:layouts)
  (sellt #:init-value 0
         #:accessor monitor-sellt
         #:setter set-.monitor-sellt!)
  (layers #:init-thunk
          (lambda ()
            (list (make-q)
                  (make-q)
                  (make-q)
                  (make-q))))
  (nmaster #:init-value 1 #:accessor monitor-nmaster)
  (mfact #:init-value 1/2 #:accessor monitor-mfact)
  (seltags #:init-value 0)
  (tagset #:init-thunk (lambda () (list 1 1)))
  (un-map #:init-value #f))

(define-method (write (o <gwwm-monitor>) port)
  (format port "#<<gwwm-monitor ~a (~a . ~a) scale: ~a>"
          (monitor-name o)
          (monitor-width o)
          (monitor-height o)
          (monitor-scale o)))

(define (monitor-at x y)
  (and=> (wlr-output-layout-output-at
          (gwwm-output-layout) x y)
         wlr-output->monitor))

(define (dirtomon dir)
  (define p wlr-output->monitor)
  (let* ((m (current-monitor))
         (area (monitor-area m)))
    (or
     (and=> (wlr-output-layout-adjacent-output
             (gwwm-output-layout)
             (bs:enum->integer %wlr-direction-enum dir)
             (monitor-output m)
             (box-x area)
             (box-y area))
            p)
     (and=> (wlr-output-layout-farthest-output
             (gwwm-output-layout)
             (logxor (bs:enum->integer %wlr-direction-enum
                                       dir) 12 ;; dir ^ (WLR_DIRECTION_LEFT|WLR_DIRECTION_RIGHT)
                                       )
             (monitor-output m)
             (box-x area)
             (box-y area))
            p)
     m)))
