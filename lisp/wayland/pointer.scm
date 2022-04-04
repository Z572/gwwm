(define-module (wayland pointer)
  #:use-module (oop goops)
  #:use-module (wayland proxy))

(define %wl-pointer-interface-struct
  (bs:struct
   `((set-cursor ,(bs:pointer '*))
     (release ,(bs:pointer '*)))))
(define-public %wl-pointer-interface
  (wayland-server->pointer "wl_pointer_interface"))

(define-method (wl-pointer-get-user-data (pointer <wl-pointer>))
  (wl-proxy-get-user-data pointer))
