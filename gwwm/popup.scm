(define-module (gwwm popup)
  #:use-module (oop goops)
  #:use-module (gwwm listener)
  #:use-module (ice-9 q)
  #:export (<gwwm-popup>
            popup-list
            wlr-xdg-popup->popup
            .popup))
(define-class <gwwm-popup> ()
  (popup #:accessor .popup #:init-keyword #:popup)
  #:metaclass <redefinable-class>)

(define-once %popup (make-q))
(define (popup-list)
  (car %popup))
(define (add-popup o)
  (q-push! %popup o))
(define (remove-popup o)
  (q-remove! %popup o))

(define-once wlr-xdg-popup->popup (make-object-property))
(define-method (initialize (o <gwwm-popup>) args)
  (let* ((obj (next-method))
         (device (.device obj)))
    (add-popup obj)
    (set! (wlr-xdg-popup->popup
           (wlr-popup-from-input-device device)) obj)
    (add-listen device 'destroy
                (lambda (listener data)
                  (set! (wlr-xdg-popup->popup device) #f)
                  (remove-popup obj)))))
