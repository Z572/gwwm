(define-module (wlroots types compositor)
  #:use-module (wayland display)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:export (wrap-wlr-compositor
            unwrap-wlr-compositor
            wlr-compositor-create))

(define-wlr-types-class wlr-compositor)

(define-wlr-procedure (wlr-compositor-create display renderer)
  ('* "wlr_compositor_create" '(* *))
  (wrap-wlr-compositor
   (% (unwrap-wl-display display)
      (unwrap-wlr-renderer renderer))))
