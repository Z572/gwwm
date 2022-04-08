(define-module (wlroots types xcursor)
  #:use-module (wayland list)
  ;; #:use-module (wlroots render renderer)
  ;; #:use-module (wlroots types output-layout)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select ((uint32 . ffi:uint32)
                                           (float . ffi:float)
                                           (int . ffi:int)
                                           %null-pointer))
  #:use-module (oop goops)
  #:export (wrap-wlr-xcursor-manager
            unwrap-wlr-xcursor-manager
            wlr-xcursor-manager-create
            wlr-xcursor-manager-load))
(define %wlr-xcursor-manager-struct
  (bs:struct `((name ,(bs:pointer *) )
               (size ,uint32)
               (scaled-themes ,%wl-list))))
(define-class <wlr-xcursor-manager> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))
(define (wrap-wlr-xcursor-manager p)
  (make <wlr-xcursor-manager> #:pointer p))
(define (unwrap-wlr-xcursor-manager o)
  (.pointer o))
(define wlr-xcursor-manager-create
  (let ((proc (wlr->procedure '* "wlr_xcursor_manager_create" (list '* ffi:uint32))))
    (lambda (name size)
      (wrap-wlr-xcursor-manager (proc (if name (string->pointer name ) %null-pointer) size)))))
(define wlr-xcursor-manager-load
  (let ((proc (wlr->procedure ffi:int "wlr_xcursor_manager_load" (list '* ffi:float))))
    (lambda (xmgr scale)
      (wrap-wlr-xcursor-manager (proc (unwrap-wlr-xcursor-manager xmgr) scale)))))
;; "wlr_xcursor_manager_load"
