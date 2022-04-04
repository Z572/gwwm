(define-module (wayland global)
  #:use-module (ice-9 format)
  #:use-module (wayland config)
  #:use-module (wayland interface)
  #:use-module (wayland util)
  #:use-module ((system foreign) #:select (null-pointer?
                                           bytevector->pointer
                                           make-pointer
                                           procedure->pointer
                                           pointer->procedure
                                           pointer->bytevector
                                           pointer->string
                                           string->pointer
                                           sizeof
                                           %null-pointer
                                           dereference-pointer
                                           define-wrapped-pointer-type
                                           pointer-address
                                           void
                                           (int . ffi:int)
                                           (uint32 . ffi:uint32)
                                           (double . ffi:double)
                                           (size_t . ffi:size_t)
                                           (uintptr_t . ffi:uintptr_t)))
  #:use-module (system foreign-object)
  #:use-module (system foreign-library)
  #:use-module (bytestructures guile)
  #:export (wl-gloabl
            wl-gloabl?
            wrap-wl-global
            unwrap-wl-global

            wl-global-create
            wl-global-remove
            wl-global-destroy))

(define-wrapped-pointer-type wl-gloabl
  wl-global?
  wrap-wl-global unwrap-wl-global
  (lambda (b p)
    (format p "#<wl-global ~x>" (pointer-address (unwrap-wl-global b)))))

(define wl-global-create
  (let ((proc (wayland-server->procedure '* "wl_global_create" (list '* '* ffi:int '* '*))))
    (lambda (display interface version data bind)
      (wrap-wl-global (proc (unwrap-wl-display display)
                            interface
                            version
                            data
                            (procedure->pointer
                             void
                             (lambda (client data version id)
                               (bind (wrap-wl-client client) data version id))
                             (list '* void ffi:uint32 ffi:uint32)))))))

(define wl-global-remove
  (let ((proc (wayland-server->procedure void "wl_global_remove" (list '*))))
    (lambda (global)
      (proc (unwrap-wl-global global)))))

(define %wl-global-destroy (wayland-server->procedure void "wl_global_destroy" (list '*)))
(define (wl-global-destroy global)
  (%wl-global-destroy (unwrap-wl-global global)))

(define %wl-global-get-interface (wayland-server->procedure '* "wl_global_get_interface" '(*)))
(define (wl-global-get-interface global)
  (pointer->wl-interface (%wl-global-get-interface (unwrap-wl-global global))))
