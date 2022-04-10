(define-module (wlroots util box)
  #:use-module (bytestructures guile)
  #:export (%wlr-box-struct %wlr-fbox-struct))
(define %wlr-box-struct
  (bs:struct `((x ,int) (y ,int) (width ,int) (height ,int))))

(define %wlr-fbox-struct
  (bs:struct `((x ,double)
               (y ,double)
               (width ,double)
               (height ,double))))
