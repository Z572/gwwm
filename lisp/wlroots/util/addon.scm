(define-module (wlroots util addon)
  #:use-module (wayland list)
  #:use-module (bytestructures guile)
  #:export (%wlr-addon-set-struct))
(define %wlr-addon-set-struct
  (bs:struct `((addons ,%wl-list))))
