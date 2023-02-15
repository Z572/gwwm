(define-module (gwwm keyboard)
  #:use-module (oop goops)
  #:use-module (wlroots types input-device)
  #:use-module (ice-9 q)
  #:use-module (gwwm listener)
  #:use-module (gwwm hooks)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (keyboard-list
            <gwwm-keyboard>
            .device))

(define-class <gwwm-keyboard> ()
  (device #:init-keyword #:device #:accessor .device)
  #:metaclass <redefinable-class>)

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

(define-method (initialize (o <gwwm-keyboard>) args)
  (let ((obj (next-method)))
    (add-keyboard obj)
    (add-listen (.device obj) 'destroy
                (lambda (listener data)
                  (run-hook cleanup-keyboard-hook obj)
                  (remove-keyboard obj)))))
