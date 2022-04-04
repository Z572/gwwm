(define-module (wayland output)
  #:use-module (wayland interface)
  #:use-module (oop goops)
  #:use-module (wayland util)
  #:export (%wl-output-interface))

(define %wl-output-interface-struct
  (bs:struct `((release ,(bs:pointer 'void)))))

(define (make-wl-output-interface-implementation release)
  (bytestructure %wl-output-interface-struct
                 `((release ,(procedure->pointer
                              void (lambda (client resource)
                                     (release (wrap-wl-client client)
                                              (wrap-wl-resource resource))) '(* *))))))

(define %wl-output-interface
  (pointer->bytestructure
   (wayland-server->pointer "wl_output_interface")
   %wl-interface))



(define-method (wl-output-get-version (output <wl-output>))
  (wl-proxy-get-version output))
