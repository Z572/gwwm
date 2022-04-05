(define-module (wayland listener)
  #:use-module (wayland list)
  #:use-module (oop goops)
  #:use-module (wayland util)
  #:use-module ((system foreign) #:select (%null-pointer
                                           procedure->pointer
                                           void))
  #:use-module (bytestructures guile)
  #:export (%wl-listener
            pointer->wl-listener
            wl-listener->pointer
            make-wl-listener
            .bytestructure
            .link
            .notify))

(define %wl-listener
  (bs:struct
   `((link ,%wl-list)
     (notify ,(bs:pointer '*)))))

;; (define-class <wl-listener> ()
;;   (pointer))

(define-class <wl-listener> ()
  (bytestructure #:accessor .bytestructure
                 #:init-keyword #:bytestructure)
  (link #:allocation #:virtual
        #:accessor .link
        #:slot-ref (lambda (a) (bytestructure-ref (.bytestructure a) 'link))
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

(define (make-wl-listener link notify)
  (make <wl-listener> #:bytestructure
        (bytestructure
         %wl-listener
         `((link ,link)
           (notify
            ,(procedure->pointer
              void
              (lambda (listener data)
                (notify (pointer->wl-listener listener) data))
              '(* *)))))))

(define (pointer->wl-listener p)
  (make <wl-listener> #:bytestructure (pointer->bytestructure p %wl-listener)))

(define (wl-listener->pointer listener)
  (bytestructure->pointer (.bytestructure listener)))