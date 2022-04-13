(define-module (wlroots types data-device)
  #:use-module (wayland display)
  ;;#:use-module (wlroots render renderer)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:export (wrap-wlr-data-source
            unwrap-wlr-data-source
            wrap-wlr-data-device-manager
            unwrap-wlr-data-device-manager
            wlr-data-device-manager-create))

(define-wlr-types-class
  wlr-data-source)

(define-wlr-types-class wlr-data-device-manager)


(define-wlr-procedure (wlr-data-device-manager-create display)
  ('* "wlr_data_device_manager_create" '(*))
  (wrap-wlr-data-device-manager (% (unwrap-wl-display display))))
