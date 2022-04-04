(define-module (wayland signal)
  #:use-module (wayland util)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (bytestructures guile)
  #:export (%wl-signal-struct
            wl-signal-add))

(define %wl-signal-struct
  (bs:struct `((listener-list ,%wl-list))))

;(define wl-signal-init)
(define (wl-signal-add signal listener)
  (wl-list-insert (bytestructure-ref signal 'listener-list 'prev)
                  (unwrap-wl-listener listener)))
(define wl-signal-get)
(define wl-signal-emit)
