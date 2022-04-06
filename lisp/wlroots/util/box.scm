(define-module (wlroots util box)
  #:use-module (bytestructures guile)
  #:export (%wlr-box-struct))
(define %wlr-box-struct
  (bs:struct `((x ,int) (y ,int) (width ,int) (height ,int))))
