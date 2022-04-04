(define-module (wayland server-core )
  #:use-module (ice-9 format)
  #:use-module (wayland config)
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
  #:use-module (bytestructures guile))


(define WL_EVENT_READABLE #x01)
(define WL_EVENT_WRITABLE #x02)
(define WL_EVENT_HANGUP   #x04)
(define WL_EVENT_ERROR    #x08)



(define-wrapped-pointer-type wl-event-loop
  wl-event-loop?
  wrap-wl-event-loop unwrap-wl-event-loop
  (lambda (b p)
    (format p "#<wl-event-loop ~x>" (pointer-address (unwrap-wl-event-loop b)))))



(define-wrapped-pointer-type wl-display
  wl-display?
  wrap-wl-display unwrap-wl-display
  (lambda (b p)
    (format p "#<wl-display ~x>" (pointer-address (unwrap-wl-display b)))))

(define %wl-message
  (bs:struct
   `((name ,char*)
     (signature ,char*)
     (types ,(bs:pointer '*)))))

(define %wl-interface
  (bs:struct
   `((name ,char*)
     (version ,int)
     (method-count ,int)
     (methods ,(bs:pointer %wl-message))
     (event-count ,int)
     (events ,(bs:pointer %wl-message)))))



;;; wl display


(define-wrapped-pointer-type wl-listener
  wl-listener?
  wrap-wl-listener unwrap-wl-listener
  (lambda (b p)
    (format p "#<wl-listener ~x>" (pointer-address (unwrap-wl-listener b)))))

(define wl-display-get-serial
  (compose (wayland-server->procedure
            ffi:uint32 "wl_display_get_serial" '(*)) unwrap-wl-display))

(define wl-display-next-serial
  (compose (wayland-server->procedure
            ffi:uint32 "wl_display_next_serial" '(*)) unwrap-wl-display))

(define wl-display-add-destory-listener
  (let ((proc (wayland-server->procedure
               void "wl_display_add_destroy_listener" '(* *))))
    (lambda (display listener)
      (proc (unwrap-wl-display display) (unwrap-wl-listener listener)))))

(define wl-display-add-client-created-listener
  (let ((proc (wayland-server->procedure
               void "wl_display_add_client_created_listener" '(* *))))
    (lambda (display listener)
      (proc (unwrap-wl-display display) (unwrap-wl-listener listener)))))







(define-wrapped-pointer-type wl-resource
  wl-resource?
  wrap-wl-resource unwrap-wl-resource
  (lambda (b p)
    (format p "#<wl-resource ~x>" (pointer-address (unwrap-wl-resource b)))))



(define %wl-listener
  (bs:struct
   `((link ,(bs:pointer '*))
     (notify ,(bs:pointer '*)))))

(define-wrapped-pointer-type wl-protocol-logger
  wl-protocol-logger?
  wrap-wl-protocol-logger unwrap-wl-protocol-logger
  (lambda (b p)
    (format p "#<wl-protocol-logger ~x>" (pointer-address (unwrap-wl-protocol-logger b)))))

(define wl-display-add-protocol-logger
  (let ((proc (wayland-server->procedure '* "wl_display_add_protocol_logger"
                                         (list '* '*))))
    (lambda (display func)
      (proc (unwrap-wl-display display) func))))

(define wl-protocol-logger-destroy
  (wayland-server->procedure
   void "wl_protocol_logger_destroy"
   (list '*))) ;;; wl_protocol_logger_func_t
