(define-module (wayland signal)
  #:use-module (wayland util)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select (pointer?))
  #:use-module (oop goops)
  #:duplicates (merge-generics)
  #:export (%wl-signal-struct
            wl-signal-add
            wrap-wl-signal
            unwrap-wl-signal
            .listener-list))

(define %wl-signal-struct
  (bs:struct `((listener-list ,%wl-list))))

                                        ;(define wl-signal-init)
(define-class <wl-signal> ()
  (bytestructure #:accessor .bytestructure #:init-keyword #:bytestructures)
  (listener-list #:allocation #:virtual
                 #:accessor .listener-list
                 #:slot-ref (lambda (a) (wrap-wl-list (bytestructure-ref (.bytestructure a) 'listener-list)))
                 #:slot-set! (lambda (instance new-val)
                               (bytestructure-set!
                                (.bytestructure instance)
                                'listener-list new-val))))
(define (wrap-wl-signal p)
  (make <wl-signal> #:bytestructures (if (pointer? p)
                                         (pointer->bytestructure (pk 'p->b p) %wl-signal-struct)
                                         p)))
(define (unwrap-wl-signal o)
  (bytestructure->pointer (.bytestructure o)))
(define (wl-signal-add signal listener)
  (wl-list-insert (wl-list-prev (.listener-list signal))
                  (.link listener)))
(define (wl-signal-init signal)
  (wl-list-init (.listener-list signal)))
(define wl-signal-get)
(define wl-signal-emit)
