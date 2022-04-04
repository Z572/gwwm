(define-module (wayland client)
  #:use-module (bytestructures guile)
  #:use-module (wayland display)
  #:use-module (wayland listener)
  #:use-module (wayland resource)
  #:use-module (wayland config)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module ((system foreign) #:select (null-pointer?
                                           bytevector->pointer
                                           make-pointer
                                           procedure->pointer
                                           define-wrapped-pointer-type
                                           pointer->procedure
                                           pointer->bytevector
                                           pointer->string
                                           string->pointer
                                           sizeof
                                           %null-pointer
                                           dereference-pointer
                                           pointer-address
                                           void
                                           (int . ffi:int)
                                           (double . ffi:double)
                                           (uint32 . ffi:uint32)
                                           (size_t . ffi:size_t)
                                           (uintptr_t . ffi:uintptr_t)))
  #:export (wl-client-create
            wrap-wl-client
            unwrap-wl-client
            wl-client-from-link
            wl-client-get-link
            wl-client-get-display))

(define-wrapped-pointer-type wl-client
  wl-client?
  wrap-wl-client unwrap-wl-client
  (lambda (b p)
    (format p "#<wl-client ~x>" (pointer-address (unwrap-wl-client b)))))

(define %wl-client-create (wayland-server->procedure '* "wl_client_create" (list '* ffi:int)))

(define (wl-client-create w-display fd)
  (wrap-wl-client (%wl-client-create (unwrap-wl-display w-display) fd)))

(define %wl-client-flush (wayland-server->procedure void "wl_client_flush" '(*)))
(define (wl-client-flush client)
  (%wl-client-flush (unwrap-wl-client client)))

(define %wl-client-get-link (wayland-server->procedure '* "wl_client_get_link" '(*)))
(define (wl-client-get-link client)
  (pointer->wl-list (%wl-client-get-link (unwrap-wl-client client))))

(define %wl-client-from-link (wayland-server->procedure '* "wl_client_from_link" '(*)))

(define (wl-client-from-link wl-l)
  (wrap-wl-client (%wl-client-from-link (wl-list->pointer wl-l))))

"wl_client_get_credentials"
(define wl-event-queue-destroy (wayland-client->procedure void "wl_event_queue_destroy" '(*)))
(define wl-client-get-object
  (let ((proc (wayland-server->procedure '* "wl_client_get_object" (list '* ffi:uint32))))
    (lambda (a b)
      (wrap-wl-resource (proc (unwrap-wl-client a) b )))))

(define wl-client-post-no-memory
  (let ((proc (wayland-server->procedure '* "wl_client_post_no_memory" (list '*))))
    (lambda (client)
      (wrap-wl-resource (proc (unwrap-wl-client client))))))

(define wl-client-post-implementation-error
  (let ((proc (wayland-server->procedure '* "wl_client_post_implementation_error" (list '* '*))))
    (lambda (client msg)
      (wrap-wl-resource (proc (unwrap-wl-client client) (string->pointer msg))))))

(define wl-client-add-resource-created-listener
  (let ((proc (wayland-server->procedure void "wl_client_add_resource_created_listener" '(* *))))
    (lambda (client listener)
      (proc (unwrap-wl-client client) (unwrap-wl-listener listener)))))

(define wl-client-for-each-resource
  (let ((proc (wayland-server->procedure void "wl_client_for_each_resource" (list '* '*))))
    (lambda (client iterator)
      (proc (unwrap-wl-client client) iterator))))
(define %wl-client-get-display (wayland-server->procedure '* "wl_client_get_display" '(*)))
(define (wl-client-get-display client)
  (wrap-wl-display (%wl-client-get-display (unwrap-wl-client client))))
