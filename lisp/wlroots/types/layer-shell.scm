(define-module (wlroots types layer-shell)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wayland display)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:export (%wlr-layer-shell-v1-struct
            wrap-wlr-layer-shell
            unwrap-wlr-layer-shell
            wlr-layer-shell-v1-create
            wl-version))

(define %wlr-layer-shell-v1-struct
  (bs:struct `((global ,(bs:pointer '*))
               (display-destroy ,%wl-listener)
               (events ,(bs:struct `((new-surface ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-class <wlr-layer-shell> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))
(define (wrap-wlr-layer-shell p)
  (make <wlr-layer-shell> #:pointer p))
(define (unwrap-wlr-layer-shell o)
  (.pointer o))
(define wlr-layer-shell-v1-create
  (let ((proc (wlr->procedure '* "wlr_layer_shell_v1_create" '(*))))
    (lambda (display)
      (wrap-wlr-layer-shell (proc (unwrap-wl-display display))))))
(define-wlr-procedure (wlr-layer-shell-v1-create display)
  ('* "wlr_layer_shell_v1_create" '(*))
  (wrap-wlr-layer-shell (% (unwrap-wl-display display))))
