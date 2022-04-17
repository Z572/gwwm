(define-module (wlroots render allocator)
  #:use-module (wayland display)
  #:use-module (wlroots utils)
  #:use-module (wlroots backend)
  #:use-module (wlroots render renderer)
  #:use-module (system foreign)
  #:use-module (oop goops)
  #:export (wrap-wlr-allocator
            unwrap-wlr-allocator
            wlr-allocator-autocreate
            wlr-allocator-destroy))

(define-class <wlr-allocator> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define (wrap-wlr-allocator p)
  (make <wlr-allocator> #:pointer p))
(define (unwrap-wlr-allocator o)
  (.pointer o))

(define-wlr-procedure (wlr-allocator-autocreate backend renderer)
  ('* "wlr_allocator_autocreate" '(* *))
  (wrap-wlr-allocator (% (unwrap-wlr-backend backend)
                         (unwrap-wlr-renderer renderer))))

(define-wlr-procedure (wlr-allocator-destroy allocator)
  (void "wlr_allocator_destroy" '(*))
  (% (unwrap-wlr-allocator allocator)))
