(define-module (wlroots types input-device)
  #:use-module (wayland signal)
  #:use-module (wlroots types)
  #:use-module (bytestructures guile)
  #:use-module (wlroots utils)
  #:export (%wlr-input-device-struct
            wrap-wlr-input-device
            unwrap-wlr-input-device))

(define-wlr-types-class wlr-input-device)
(define %wlr-input-device-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (type ,int)
               (vendor ,unsigned-int)
               (product ,unsigned-int)
               (name ,cstring-pointer)
               (width-mm ,double)
               (height-mm ,double)
               (output-name ,cstring-pointer)
               (union ,(bs:union `((_device ,(bs:pointer 'void))
                                   (keyboard ,(bs:pointer '*))
                                   (pointer ,(bs:pointer '*))
                                   (switch-device ,(bs:pointer '*))
                                   (touch ,(bs:pointer '*))
                                   (tablet ,(bs:pointer '*))
                                   (tablet-pad ,(bs:pointer '*)))))
               (events ,(bs:struct `((destroy ,%wl-signal-struct)))))))
