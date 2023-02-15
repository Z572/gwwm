(define-module (gwwm pointer)
  #:use-module (oop goops)
  #:use-module (gwwm listener)
  #:use-module (wlroots types input-device)
  #:use-module (ice-9 q)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (pointer-list
            <gwwm-pointer>
            .device))

(define-class <gwwm-pointer> ()
  (device #:init-keyword #:device #:accessor .device)
  #:metaclass <redefinable-class>)

(define-once %pointers (make-q))
(define (pointer-list)
  (car %pointers))
(define (add-pointer o)
  (q-push! %pointers o))
(define (remove-pointer o)
  (q-remove! %pointers o))
(define-method (initialize (o <gwwm-pointer>) args)
  (let ((obj (next-method)))
    (add-pointer obj)
    (add-listen (.device obj) 'destroy
                (lambda (listener data)
                  (remove-pointer obj)))))
