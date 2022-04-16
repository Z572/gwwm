(define-module (wlroots types output-layout)
  #:use-module (wayland display)
  ;;#:use-module (wlroots render renderer)
  #:use-module (wlroots utils)
  #:use-module (wlroots types)
  #:use-module (oop goops)
  #:export (wrap-wlr-output-layout
            unwrap-wlr-output-layout
            wlr-output-layout-create))

(define-wlr-types-class wlr-output-layout)

(define-wlr-procedure (wlr-output-layout-create)
  ('* "wlr_output_layout_create" '())
  (wrap-wlr-output-layout (%)))
