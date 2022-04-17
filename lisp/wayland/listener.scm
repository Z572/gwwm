(define-module (wayland listener)
  #:use-module (wayland list)
  #:use-module (oop goops)
  #:use-module (wayland util)
  #:use-module ((system foreign) #:select (%null-pointer
                                           procedure->pointer
                                           void
                                           pointer?
                                           pointer->bytevector))
  #:use-module (bytestructures guile)
  #:duplicates (merge-generics)
  #:export (%wl-listener
            wrap-wl-listener
            unwrap-wl-listener
            make-wl-listener
            .bytestructure
            .link
            .notify))

(define wl-notify-func
  (bs:pointer
   (delay (bs:struct `((listener ,%wl-listener)
                       (data ,(bs:pointer 'void)))))))
(define %wl-listener
  (bs:struct
   `((link ,%wl-list)
     (notify ,wl-notify-func))))


;; (define-class <wl-listener> ()
;;   (pointer))

(define-class <wl-listener> ()
  (bytestructure #:accessor .bytestructure
                 #:init-keyword #:bytestructure)
  (link #:allocation #:virtual
        #:accessor .link
        #:slot-ref (lambda (a) (wrap-wl-list (bytestructure-ref (.bytestructure a) 'link)))
        #:slot-set! (lambda (instance new-val)
                      (bytestructure-set!
                       (.bytestructure instance)
                       'link new-val)))
  (notify #:allocation #:virtual
          #:accessor .notify
          #:slot-ref (lambda (a) (bytestructure-ref (.bytestructure a) 'notify))
          #:slot-set! (lambda (instance new-val)
                        (bytestructure-set!
                         (.bytestructure instance)
                         'notify new-val))))

(define (make-wl-listener ;; link
         notify)
  ;; (pk 'link-1 link notify)
  (make <wl-listener> #:bytestructure
        (pk 'ss (bytestructure
                 %wl-listener
                 `((link ,(bytestructure-bytevector (bytestructure %wl-list)))
                   (notify
                    ,(procedure->pointer
                      void
                      (lambda (listener data)
                        (pk 'listener listener data)
                        (notify (wrap-wl-listener listener) data))
                      '(* *))))))))

(define (wrap-wl-listener p)
  (make <wl-listener> #:bytestructure (cond ((pointer? p ) (pointer->bytestructure p %wl-listener))
                                            ((bytestructure? p) p))))

(define (unwrap-wl-listener listener)
  (bytestructure->pointer (.bytestructure listener)))
