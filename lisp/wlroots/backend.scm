(define-module (wlroots backend)
  #:use-module (wlroots config)
  #:use-module (wayland signal)
  #:use-module (wayland display)
  #:use-module (system foreign)
  #:use-module (oop goops)
  #:use-module (bytestructures guile)
  #:export (wlr-backend-autocreate
            unwrap-wlr-backend
            wrap-wlr-backend))
(define (wlr->pointer name)
  (dynamic-func name (dynamic-link %libwlroots)))
(define (wlr->procedure return name params)
  (let ((ptr (wlr->pointer name)))
    (pointer->procedure return ptr params)))

(define-class <wlr-backend> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define (wrap-wlr-backend p)
  (make <wlr-backend> #:pointer p))
(define (unwrap-wlr-backend o)
  (.pointer o))
(define %wlr-backend-struct
  (bs:struct
   `((wlr-backend-impl ,(bs:pointer '*))
     (events ,(bs:struct
               `((destroy ,%wl-signal-struct)))))))
(define (wlr-backend-autocreate display)
  (wrap-wlr-backend
   ((wlr->procedure
     '* "wlr_backend_autocreate" (list '*))
    (unwrap-wl-display display))))
