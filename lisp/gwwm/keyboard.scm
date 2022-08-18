(define-module (gwwm keyboard)
  #:use-module (oop goops)
  #:use-module (wlroots types input-device)
  #:export (keyboard-input-device
            keyboard-list <gwwm-keyboard>))

;;; XXX: are we should really call it keyboard.
(define-class <gwwm-keyboard> ()
  (data #:init-keyword #:data #:accessor .data))

(define-method (keyboard-input-device (keyboard <gwwm-keyboard>))
  ((@@ (gwwm) keyboard-input-device) keyboard))

(define (keyboard-list)
  ((@@ (gwwm) keyboard-list)))

(define-method (write (o <gwwm-keyboard>) port)
  (format port "#<<gwwm-keyboard> ~S>"
          (wlr-input-device-name (keyboard-input-device o))))
