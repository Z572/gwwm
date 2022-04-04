(define-module (wayland cursor)
  #:use-module (wayland config)
  #:use-module (bytestructures guile)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
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
                                           (double . ffi:double)
                                           (size_t . ffi:size_t)
                                           (uintptr_t . ffi:uintptr_t))))
(define wl-cursor-image
  (bs:struct `((width ,uint32)
               (height ,uint32)
               (hotspot-x ,uint32)
               (hotspot-y ,uint32)
               (delay ,uint32))))
(define wl-cursor
  (bs:struct
   `((image-count ,unsigned-int)
     (images ,(bs:pointer wl-cursor-image))
     (name ,(bs:pointer int8)))))
(define (libwayland-cursor->procedure return name params)
  (catch #t
    (lambda ()
      (let ((ptr (dynamic-func name (dynamic-link %libwayland-cursor))))
        (pointer->procedure return ptr params)))
    (lambda args
      (lambda _
        (throw 'system-error name  "~A" (list (strerror ENOSYS))
               (list ENOSYS))))))
(define (wl-cursor-theme-load name size shm)
  (let ((proc (libwayland-cursor->procedure '* "wl_cursor_theme_load" (list '* ffi:int '*))))
    (proc (string->pointer name) size shm)))
