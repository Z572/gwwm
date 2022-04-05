(define-module (wlroots types compositor)
  #:use-module (wayland display)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:export (wrap-wlr-compositor
            unwrap-wlr-compositor
            wlr-compositor-create))

(define-class <wlr-compositor> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define (wrap-wlr-compositor p)
  (make <wlr-compositor> #:pointer p))
(define (unwrap-wlr-compositor o)
  (.pointer o))

(define wlr-compositor-create
  (let ((proc (wlr->procedure '* "wlr_compositor_create" '(* *))))
    (lambda (display renderer)
      (wrap-wlr-compositor (proc (unwrap-wl-display display) (unwrap-wlr-renderer renderer))))))
