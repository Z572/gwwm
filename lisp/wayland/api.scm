(define-module (wayland api)
  #:use-module (system foreign)
  #:use-module (system foreign-object)
  #:use-module (system foreign-library)
  #:use-module (oop goops))

(define libwayland-client
  (dynamic-link
   "/gnu/store/pf4ahy3r4m1cdl06znir8x6698ylvdjr-wayland-1.18.0/lib/libwayland-server.so"))

(define-foreign-object-type <wl-message>
  make-wl-message
  (name signature))

(define-foreign-object-type <wl-list>
  make-wl-list
  (list))

(foreign-library-function
 "/gnu/store/pf4ahy3r4m1cdl06znir8x6698ylvdjr-wayland-1.18.0/lib/libwayland-server"
 "wl_event_loop_destroy" #:return-type void #:arg-types (list '*))
"wl_client_add_resource"
(define wl-event-loop-create
  (foreign-library-function
   #f ""
   #:return-type 'pointer))

(define-method (wl-list-init (l <list>))
  (make-wl-list l))

(define-class <wl-client> ()
  (->client #:accessor ->client #:init-keyword #:->client)
  (regions #:accessor regions #:init-keyword #:regions)
  (resources #:accessor resources #:init-keyword #:resources)
  (pointer #:accessor pointer #:init-keyword #:pointer)
  (keyboard #:accessor keyboard #:init-keyword #:keyboard))

(define (finalize-file file)
  (let ((fd (struct-ref file 0)))
    (unless (< fd 0)
      (struct-set! file 0 -1)
      (close-fdes fd))))

(define <file>
  (make-foreign-object-type '<file> '(fd)
                            #:finalizer finalize-file))

(define (make-file fd)
  (make <file> #:fd fd))
