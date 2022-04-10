(define-module (wayland list)
  #:use-module (oop goops)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select (make-pointer %null-pointer void pointer?(int . ffi:int)))
  #:export (;wl-list-init
            %wl-list
            wrap-wl-list
            unwrap-wl-list
            wl-list-init
            wl-list-insert
            wl-list-remove
            wl-list-length
            wl-list-empty
            make-wl-list
            .bytestructure
            wl-list-bytestructure))

;; (define-class <wl-list> ()
;;   (pointer #:ass))
(define %wl-list
  (bs:struct
   `((prev ,(bs:pointer (delay %wl-list)))
     (next ,(bs:pointer (delay %wl-list))))))

(define-class <wl-list> ()
  (bytestructure #:accessor .bytestructure #:init-keyword #:bytestructure))

(define (wrap-wl-list p)
  (make <wl-list> #:bytestructure (if (pointer? p) (pointer->bytestructure p %wl-list) p)))

(define (make-wl-list )
  (wrap-wl-list (bytestructure %wl-list)))
(define (unwrap-wl-list o)
  (bytestructure->pointer (.bytestructure o)))

(define (wl-list-bytestructure o) (.bytestructure o))

(define-public (wl-list-next wl-l)
  (wrap-wl-list
   (make-pointer
    (bytestructure-ref
     (pointer->bytestructure (unwrap-wl-list wl-l) %wl-list) 'next))))

(define-public (wl-list-prev wl-l)
  (wrap-wl-list
   (make-pointer
    (bytestructure-ref
     (pointer->bytestructure (unwrap-wl-list wl-l) %wl-list) 'prev))))

(define %wl-list-init
  (wayland-server->procedure
   void "wl_list_init"
   (list '*)))

(define (wl-list-init wl-l)
  (let ((p (unwrap-wl-list wl-l)))
    (%wl-list-init p)
    (pk 'wl-list-init (wrap-wl-list p))))

(define wl-list-insert
  (let ((proc (wayland-server->procedure
               void "wl_list_insert"
               (list '* '*))))
    (lambda (lst lst2)
      "XXX: need fix it"
      (proc (unwrap-wl-list lst)
            (unwrap-wl-list lst2)))))

(define wl-list-remove
  (let ((proc (wayland-server->procedure
               void "wl_list_remove"
               (list '*))))
    (lambda (l)
      (proc (unwrap-wl-list l)))))

(define %wl-list-length
  (wayland-server->procedure ffi:int "wl_list_length" '(*)))

(define (wl-list-length w-list)
  (%wl-list-length (unwrap-wl-list w-list)))

(define %wl-list-empty (wayland-server->procedure ffi:int "wl_list_empty" '(*)))

(define (wl-list-empty l) (case (%wl-list-empty (unwrap-wl-list l))
                            ((0) #f)
                            ((1) #t)
                            (else (error ))) )
