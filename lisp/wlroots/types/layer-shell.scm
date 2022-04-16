(define-module (wlroots types layer-shell)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wayland display)
  #:use-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:export (%wlr-layer-shell-v1-struct
            wrap-wlr-layer-shell
            unwrap-wlr-layer-shell
            wlr-layer-shell-v1-create
            wl-version
            get-event-signal))

(define %wlr-layer-shell-v1-struct
  (bs:struct `((global ,(bs:pointer '*))
               (display-destroy ,%wl-listener)
               (events ,(bs:struct `((new-surface ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-wlr-types-class wlr-layer-shell)

(define-wlr-procedure (wlr-layer-shell-v1-create display)
  ('* "wlr_layer_shell_v1_create" '(*))
  (wrap-wlr-layer-shell (% (unwrap-wl-display display))))

(define-method (get-event-signal (b <wlr-layer-shell>) (signal-name <symbol>))
  (wrap-wl-signal (bytestructure-ref
                   (pointer->bytestructure
                    (unwrap-wlr-layer-shell b)
                    %wlr-layer-shell-v1-struct)
                   'events  signal-name)))
