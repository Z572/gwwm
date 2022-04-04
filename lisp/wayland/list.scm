(define-module (wayland list)
  #:use-module (oop goops)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select (make-pointer void (int . ffi:int)))
  #:export (;wl-list-init
            %wl-list
            pointer->wl-list
            wl-list->pointer
            wl-list-init
            wl-list-insert
            wl-list-remove
            wl-list-length
            wl-list-empty))

;; (define-class <wl-list> ()
;;   (pointer #:ass))
(define %wl-list
  (bs:struct
   `((prev ,(bs:pointer (delay %wl-list)))
     (next ,(bs:pointer (delay %wl-list))))))

(define-class <wl-list> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define-public (wl-list-next wl-l)
  (pointer->wl-list
   (make-pointer
    (bytestructure-ref
     (pointer->bytestructure (.pointer wl-l) %wl-list) 'next))))

(define-public (wl-list-prev wl-l)
  (pointer->wl-list
   (make-pointer
    (bytestructure-ref
     (pointer->bytestructure (.pointer wl-l) %wl-list) 'prev))))

(define (pointer->wl-list p)
  (make <wl-list> #:pointer p))

(define (wl-list->pointer wl-l)
  (.pointer wl-l))

(define %wl-list-init
  (wayland-server->procedure
   void "wl_list_init"
   (list '*)))

(define (wl-list-init wl-l)
  (let ((p (wl-list->pointer wl-l)))
    (%wl-list-init p)))

(define (wl-list-insert lst)
  (wayland-server->procedure
   void "wl_list_insert"
   (list '* '*)))

(define (wl-list-remove lst)
  (wayland-server->procedure
   void "wl_list_remove"
   (list '*)))

(define %wl-list-length
  (wayland-server->procedure ffi:int "wl_list_length" '(*)))

(define (wl-list-length w-list)
  (%wl-list-length (wl-list->pointer w-list)))

(define %wl-list-empty (wayland-server->procedure ffi:int "wl_list_empty" '(*)))

(define (wl-list-empty l) (%wl-list-empty (wl-list->pointer l)) )
