(define-module (wayland signal)
  #:use-module (wayland util)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (rnrs bytevectors)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select (pointer? pointer->bytevector bytevector->pointer make-pointer))
  #:use-module (oop goops)
  #:duplicates (merge-generics)
  #:export (%wl-signal-struct
            wl-signal-add
            wrap-wl-signal
            unwrap-wl-signal
            wl-signal-init
            .listener-list))

(define %wl-signal-struct
  (bs:struct `((listener-list ,(bs:pointer %wl-list)))))

(pk 'a-1)
(define-bytestructure-accessors %wl-signal-struct
  wl-signal-unwrap wl-signal-ref wl-signal-set!)
(pk 'a)
(define-class <wl-signal> ()
  (bytevectory #:accessor .bytevectory #:init-keyword #:bytevectory)
  (listener-list #:allocation #:virtual
                 #:accessor .listener-list
                 #:slot-ref (lambda (a)
                              (wrap-wl-list
                               (make-pointer (wl-signal-ref (.bytevectory a) listener-list))))
                 #:slot-set! (lambda (instance new-val)
                               (wl-signal-set!
                                (.bytevectory instance)
                                listener-list (unwrap-wl-list new-val))
                               )))
(define (wrap-wl-signal p)
  (make <wl-signal> #:bytevectory
        (cond ((pointer? p)
               (pointer->bytevector p (bytestructure-descriptor-size %wl-signal-struct)))
              ((bytestructure? p)
               (bytestructure-bytevector p))
              (else p))))
(define (make-wl-signal)
  (wrap-wl-signal
   (make-bytevector
    (bytestructure-descriptor-size %wl-signal-struct))))
(define (unwrap-wl-signal o)
  (bytevector->pointer (.bytevectory o))
  ;; (bytestructure->pointer (.bytestructure o))
  )
(define (wl-signal-add signal listener)
  (wl-list-insert (wl-list-prev (.listener-list signal))
                  (.link listener)))
(define* (wl-signal-init #:optional (signal (make-wl-signal)))
  (wl-list-init (.listener-list signal))
  signal)
(define wl-signal-get)
(define wl-signal-emit)
