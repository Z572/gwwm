(define-module (wlroots types data-device)
  #:use-module (wayland display)
  ;;#:use-module (wlroots render renderer)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:export (wrap-wlr-data-device-manager
            unwrap-wlr-data-device-manager
            wlr-data-device-manager-create))

(define-class <wlr-data-device-manager> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define (wrap-wlr-data-device-manager p)
  (make <wlr-data-device-manager> #:pointer p))
(define (unwrap-wlr-data-device-manager o)
  (.pointer o))

(define wlr-data-device-manager-create
  (let ((proc (wlr->procedure '* "wlr_data_device_manager_create" '(*))))
    (lambda (display)
      (wrap-wlr-data-device-manager (proc (unwrap-wl-display display) )))))
