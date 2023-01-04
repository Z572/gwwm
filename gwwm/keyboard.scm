(define-module (gwwm keyboard)
  #:use-module (oop goops)
  #:use-module (wlroots types input-device)
  #:use-module (ice-9 q)
  #:export (keyboard-input-device
            keyboard-list <gwwm-keyboard>
            .device))

;;; XXX: are we should really call it keyboard.
(define-class <gwwm-keyboard> ()
  (device #:init-keyword #:device #:accessor .device))

(define-once %keyboards (make-q))

(define (add-keyboard keyboard)
  (q-push! %keyboards keyboard ))

(define (remove-keyboard keyboard)
  (q-remove!  %keyboards keyboard))

(define (keyboard-list)
  (car %keyboards))

(define-method (write (o <gwwm-keyboard>) port)
  (format port "#<<gwwm-keyboard> ~S>"
          (wlr-input-device-name (.device o))))
