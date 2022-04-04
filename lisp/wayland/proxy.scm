(define-module (wayland proxy)
  #:use-module (ice-9 format)
  #:use-module (wayland util)
  #:use-module (oop goops)
  #:use-module ((system foreign) #:select (void
                                           make-pointer
                                           %null-pointer
                                           pointer-address
                                           procedure->pointer
                                           define-wrapped-pointer-type
                                           (uint32 . ffi:uint32)
                                           (int . ffi:int)))
  #:export (<wl-proxy>
            .pointer
            wrap-wl-proxy
            wl-proxy-get-class
            wl-proxy-add-listener
            wl-proxy-marshal-constructor
            wl-proxy-marshal-constructor-versioned
            wl-proxy-get-user-data
            wl-proxy-get-version))

;; (define-wrapped-pointer-type wl-proxy
;;   wl-proxy?
;;   wrap-wl-proxy unwrap-wl-proxy
;;   (lambda (b p)
;;     (format p "#<wl-proxy ~x>" (pointer-address (unwrap-wl-proxy b)))))

(define-class <wl-proxy> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define (wrap-wl-proxy p)
  (make <wl-proxy> #:pointer p))
(define wl-proxy->pointer .pointer)
(define %wl-proxy-create (wayland-client->procedure '* "wl_proxy_create" '(* *)))

(define-method (wl-proxy-create (factory <wl-proxy>) interface)
  (make <wl-proxy> #:pointer (%wl-proxy-create (.pointer factory)
                                               interface)))

(define %wl-proxy-get-class (wayland-client->procedure '* "wl_proxy_get_class" '(*)))
(define-method (wl-proxy-get-class (proxy <wl-proxy>))
  (pointer->string (%wl-proxy-get-class (.pointer proxy))))
(define %wl-proxy-marshal-constructor
  (wayland-client->procedure
   '*
   "wl_proxy_marshal_constructor"
   (list '* ffi:uint32 '*)))

(define-method (wl-proxy-marshal-constructor (proxy <wl-proxy>) opcode interface)
  (%wl-proxy-marshal-constructor
   (.pointer proxy)
   opcode
   interface))

(define %wl-proxy-marshal-constructor-versioned
  (wayland-client->procedure
   '*
   "wl_proxy_marshal_constructor_versioned"
   (list '* ffi:uint32 '* ffi:uint32 ffi:uint32 '* ffi:uint32 '*)))
(define-method (wl-proxy-marshal-constructor-versioned (proxy <wl-proxy>) opcode interface version . other)
  (apply %wl-proxy-marshal-constructor-versioned
         (.pointer proxy)
         opcode
         interface
         version
         other))

(define %wl-proxy-add-listener
  (wayland-client->procedure ffi:int "wl_proxy_add_listener" (list '* '* '*)))
(define-method (wl-proxy-add-listener (proxy <wl-proxy>) implementation data)
  (%wl-proxy-add-listener
   (.pointer proxy)
   implementation
   data
   ))

;; (define %wl-proxy-get-listener
;;   (wayland-client->procedure
;;    '* "wl_proxy_get_listener" '(*)))

;; (define-method (wl-proxy-get-listener (proxy (<wl-proxy>)))
;;   (let ((p (.pointer proxy)))
;;     (%wl-proxy-get-listener p)))
(define %wl-proxy-get-version
  (wayland-client->procedure
   ffi:uint32 "wl_proxy_get_version" '(*)))

(define-method (wl-proxy-get-version (proxy <wl-proxy>))
  (%wl-proxy-get-version (.pointer proxy)))

(define %wl-proxy-get-user-data
  (wayland-client->procedure
   '* "wl_proxy_get_user_data" '(*)))

(define-method (wl-proxy-get-user-data (proxy <wl-proxy>))
  (%wl-proxy-get-user-data (.pointer proxy)))
