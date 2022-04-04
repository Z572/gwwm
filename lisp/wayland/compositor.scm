(define-module (wayland compositor)
  #:use-module (system foregign-object))
                                        ;(define)

;; (define-foreign-object-type
;;   <wl-compositor> make-wl-compositor ())
(define WL_COMPOSITOR_CREATE_SURFACE 0)
(define (wl-compositor-create-surface compositior)
  (wl-proxy-marshal-constructor
   compositior
   WL_COMPOSITOR_CREATE_SURFACE
   (wl-interface->pointer wl-surface-in)))
