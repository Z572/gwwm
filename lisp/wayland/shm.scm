(define-module (wayland shm)
  #:use-module ((system foreign) #:select (null-pointer?
                                           define-wrapped-pointer-type
                                           pointer->procedure
                                           void
                                           pointer-address
                                           (int32 . ffi:int32)
                                           (uint32 . ffi:uint32)
                                           (int . ffi:int)))
  #:use-module (wayland util)
  #:use-module (wayland proxy)
  #:use-module (wayland client)
  #:use-module (wayland resource)
  #:use-module (ice-9 format)
  #:use-module (bytestructures guile))

(define WL_SHM_CREATE_POOL 0)
(define %wl-shm-pool-interface-struct
  (bs:struct
   `((create-buffer ,(bs:pointer '*))
     (destroy ,(bs:pointer '*))
     (resize ,(bs:pointer '*)))))

(define %wl-shm-pool-interface
  (pointer->bytestructure
   (wayland-server->pointer "wl_registry_interface")
   %wl-shm-pool-interface-struct))

(define (wl-shm-poll-create-buffer client resource id offset width height stride format)
  (let ((proc (pointer->procedure
               void
               (bytestructure-ref %wl-shm-pool-interface 'create-buffer)
               (list '* '*
                     ffi:uint32
                     ffi:int32
                     ffi:int32
                     ffi:int32
                     ffi:int32
                     ffi:uint32))))
    (proc (unwrap-wl-client client) resource
          id offset
          width height
          stride format)))

(define-wrapped-pointer-type wl-shm-buffer
  wl-shm-buffer?
  wrap-wl-shm-buffer unwrap-wl-shm-buffer
  (lambda (b p)
    (format p "#<wl-shm-buffer ~x>" (pointer-address (unwrap-wl-shm-buffer b)))))

(define %wl-shm-buffer-get
  (wayland-server->procedure '* "wl_shm_buffer_get" '(*)))
(define (wl-shm-buffer-get resource)
  (wrap-wl-shm-buffer (%wl-shm-buffer-get (unwrap-wl-resource resource))))

(define %wl-shm-buffer-begin-access
  (wayland-server->procedure void "wl_shm_buffer_begin_access" '(*)))

(define (wl-shm-buffer-begin-access buffer)
  (%wl-shm-buffer-begin-access (unwrap-wl-shm-buffer buffer)))
(define %wl-shm-buffer-end-access
  (wayland-server->procedure '* "wl_shm_buffer_end_access" '(*)))
(define (wl-shm-buffer-end-access resource)
  (%wl-shm-buffer-end-access (unwrap-wl-resource resource)))

(define %wl-shm-buffer-get-data
  (wayland-server->procedure '* "wl_shm_buffer_get" '(*)))
(define (wl-shm-buffer-get-data resource)
  (%wl-shm-buffer-get (unwrap-wl-resource resource)))

(define %wl-shm-buffer-get-stride (wayland-server->procedure '* "wl_shm_buffer_get" '(*)))
(define (wl-shm-buffer-get-stride resource)
  (%wl-shm-buffer-get (unwrap-wl-resource resource)))

(define %wl-shm-buffer-get-format
  (wayland-server->procedure '* "wl_shm_buffer_get_format" '(*)))
(define (wl-shm-buffer-get buffer)
  (%wl-shm-buffer-get (unwrap-wl-shm-buffer buffer)))

(define (wl-shm-create-pool shm fd size)
  (wl-proxy-marshal-constructor
   ( shm)
   WL_SHM_BUFFER_GET_FORMAT
   %wl-shm-pool-interface %null-pointer fd size ))
