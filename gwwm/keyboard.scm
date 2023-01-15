(define-module (gwwm keyboard)
  #:use-module (oop goops)
  #:use-module (wlroots types input-device)
  #:use-module (ice-9 q)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (keyboard-list
            <gwwm-keyboard>
            .device))

(define-class <gwwm-keyboard> ()
  (device #:init-keyword #:device #:accessor .device))

(define-once %keyboards (make-q))

(define (add-keyboard keyboard)
  (q-push! %keyboards keyboard))

(define (remove-keyboard keyboard)
  (q-remove! %keyboards keyboard))

(define (keyboard-list)
  (car %keyboards))

(define-method (write (o <gwwm-keyboard>) port)
  (format port "#<<gwwm-keyboard> ~S>"
          (.name (.device o))))
