(define-module (gwwm touch)
  #:use-module (oop goops)
  #:use-module (gwwm listener)
  #:use-module (ice-9 q)
  #:export (<gwwm-touch>
            <gwwm-touch-point>
            touch-list
            wlr-touch->touch
            .device))
(define-class <gwwm-touch> ()
  (device #:accessor .device #:init-keyword #:device)
  #:metaclass <redefinable-class>)

(define-class <gwwm-touch-point> ()
  (touch-id #:init-keyword #:touch-id)
  (x #:init-keyword #:x)
  (y #:init-keyword #:y)
  #:metaclass <redefinable-class>)

(define-once %touch (make-q))
(define (touch-list)
  (car %touch))
(define (add-touch o)
  (q-push! %touch o))
(define (remove-touch o)
  (q-remove! %touch o))

(define-once wlr-touch->touch (make-object-property))
(define-method (initialize (o <gwwm-touch>) args)
  (let* ((obj (next-method))
         (device (.device)))
    (add-touch obj)
    (set! (wlr-touch->touch device) obj)
    (add-listen device 'destroy
                (lambda (listener data)
                  (set! (wlr-touch->touch device) #f)
                  (remove-touch obj)))))
