(define-module (wlroots types output-layout)
  #:use-module (wayland display)
  ;;#:use-module (wlroots render renderer)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:export (wrap-wlr-output-layout
            unwrap-wlr-output-layout
            wlr-output-layout-create))

(define-class <wlr-output-layout> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define (wrap-wlr-output-layout p)
  (make <wlr-output-layout> #:pointer p))
(define (unwrap-wlr-output-layout o)
  (.pointer o))

(define wlr-output-layout-create
  (let ((proc (wlr->procedure '* "wlr_output_layout_create" '())))
    (lambda ()
      (wrap-wlr-output-layout (proc)))))
