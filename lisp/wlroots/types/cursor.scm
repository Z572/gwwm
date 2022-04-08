(define-module (wlroots types cursor)
  #:use-module (wayland display)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots utils)
  #:use-module (system foreign)
  #:use-module (oop goops)
  #:export (wrap-wlr-cursor
            unwrap-wlr-cursor
            wlr-cursor-create
            wlr-cursor-attach-output-layout))

(define-class <wlr-cursor> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define (wrap-wlr-cursor p)
  (make <wlr-cursor> #:pointer p))
(define (unwrap-wlr-cursor o)
  (.pointer o))

(define wlr-cursor-create
  (let ((proc (wlr->procedure '* "wlr_cursor_create" '())))
    (lambda ()
      (wrap-wlr-cursor (proc)))))

(define wlr-cursor-attach-output-layout
  (let ((proc (wlr->procedure int "wlr_cursor_attach_output_layout" '(* *))))
    (lambda (cursor output-layout)
      (proc (unwrap-wlr-cursor cursor) (unwrap-wlr-output-layout output-layout)))))
