(define-module (wayland event-loop)
  #:use-module (ice-9 format)
  #:use-module (wayland config)
  #:use-module (wayland util)
  #:use-module (wayland interface)
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
  #:export (wrap-wl-event-loop
            unwrap-wl-event-loop
            wl-event-loop?

            wl-event-loop-create
            wl-event-loop-destroy
            wl-event-loop-add-timer
            wl-event-loop-add-fd
            wl-event-loop-add-signal
            wl-event-loop-dispatch))

(define-wrapped-pointer-type wl-event-loop
  wl-event-loop?
  wrap-wl-event-loop unwrap-wl-event-loop
  (lambda (b p)
    (format p "#<wl-event-loop ~x>" (pointer-address (unwrap-wl-event-loop b)))))

(define-wrapped-pointer-type wl-event-source
  wl-event-source?
  wrap-wl-event-source unwrap-wl-event-source
  (lambda (b p)
    (format p "#<wl-event-source ~x>" (pointer-address (unwrap-wl-event-source b)))))

(define (make-interface)
  (bytestructure %wl-interface))

(define wl-event-loop-create
  (compose wrap-wl-event-loop
           (wayland-server->procedure
            '* "wl_event_loop_create" '())))

(define wl-event-loop-destroy
  (compose (wayland-server->procedure void "wl_event_loop_destroy" '(*))
           unwrap-wl-event-loop))

;;;
(define %wl-event-loop-add-fd
  (wayland-server->procedure
   void
   "wl_event_loop_add_fd"
   (list '*
         ffi:int
         ffi:uint32
         '* ;; type of wl_event_loop_fd_func_t
         '*)))
(define (wl-event-loop-add-fd loop fd mask func data)
  (%wl-event-loop-add-fd
   loop fd mask
   (procedure->pointer int (lambda (a b c) (func a b c))
                       (list ffi:int ffi:uint32 '*))
   data))


(define wl-event-source-fd-update
  (wayland-server->procedure
   ffi:int "wl_event_source_fd_update"
   (list '* ffi:uint32)))

(define* wl-event-loop-add-timer
  (let ((proc (wayland-server->procedure
               '* "wl_event_loop_add_timer"
               (list '* '* ;; type of wl_event_loop_fd_func_t
                     '*))))
    (lambda* (loop func #:optional (data %null-pointer))
      (wrap-wl-event-source
       (proc
        (unwrap-wl-event-loop loop)
        (procedure->pointer ffi:int
                            (lambda (a) (func a))
                            '(*))
        data)))))

(define wl-event-loop-add-signal
  (compose wrap-wl-event-source
           (wayland-server->procedure
            '* "wl_event_loop_add_signal"
            (list ffi:int '* ;; type of wl_event_loop_fd_func_t
                  '*))))

(define %wl-event-loop-dispatch
  (wayland-server->procedure
   ffi:int "wl_event_loop_dispatch"
   (list '* ffi:int)))

(define (wl-event-loop-dispatch loop timeout)
  (%wl-event-loop-dispatch (unwrap-wl-event-loop loop) timeout ))
(define wl-event-loop-dispatch-idle
  (wayland-server->procedure
   void "wl_event_loop_dispatch_idle"
   (list '*)))

(define wl-event-source-timer-update
  (wayland-server->procedure
   ffi:int "wl_event_source_timer_update" (list '* ffi:int )))

(define wl-event-source-remove
  (compose (wayland-server->procedure
            void "wl_event_source_remove"
            (list '*)) unwrap-wl-event-source))

(define wl-event-source-check
  (compose (wayland-server->procedure
            void "wl_event_source_check"
            (list '*)) unwrap-wl-event-source))
(define %wl-event-loop-get-fd
  (wayland-server->procedure ffi:int "wl_event_loop_get_fd" '(*)))
