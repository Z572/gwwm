(define-module (wlroots render renderer)
  #:use-module (wayland display)
  #:use-module (wlroots utils)
  #:use-module (wlroots backend)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:export (wrap-wlr-renderer unwrap-wlr-renderer wlr-renderer-autocreate
                              wlr-renderer-init-wl-display))

(define-class <wlr-renderer> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define (wrap-wlr-renderer p)
  (make <wlr-renderer> #:pointer p))
(define (unwrap-wlr-renderer o)
  (.pointer o))

(define wlr-renderer-autocreate
  (let ((proc (wlr->procedure '* "wlr_renderer_autocreate" (list '*))))
    (lambda (backend)
      (wrap-wlr-renderer (proc (unwrap-wlr-backend backend))))))
(define wlr-renderer-init-wl-display
  (let ((proc (wlr->procedure ffi:int "wlr_renderer_init_wl_display" '(* *))))
    (lambda (renderer display)
      (proc (unwrap-wlr-renderer renderer) (unwrap-wl-display display)))))
