(define-module (wlroots types seat)
  #:use-module (wayland list)
  #:use-module (wayland display)
  ;; #:use-module (wlroots render renderer)
  ;; #:use-module (wlroots types output-layout)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select ((uint32 . ffi:uint32)
                                           (float . ffi:float)
                                           (int . ffi:int)
                                           %null-pointer
                                           string->pointer))
  #:use-module (oop goops)
  #:export (wrap-wlr-seat
            unwrap-wlr-seat
            wlr-seat-create))

(define-class <wlr-seat> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))
(define (wrap-wlr-seat p)
  (make <wlr-seat> #:pointer p))
(define (unwrap-wlr-seat o)
  (.pointer o))

(define wlr-seat-create
  (let ((proc (wlr->procedure '* "wlr_seat_create" '(* *))))
    (lambda (display name)
      (wrap-wlr-seat (proc (unwrap-wl-display display)
                           (string->pointer name ))))))
