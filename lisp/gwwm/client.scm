(define-module (gwwm client)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:export (current-client
            client-floating?
            client-set-floating!
            client-is-floating?
            client-fullscreen?
            client-is-fullscreen?
            client-set-fullscreen!
            client-toggle-fullscreen
            client-list
            client-get-appid
            client-live?
            client=?
            <gwwm-client>))

(define-class <gwwm-client> ()
  (data #:init-keyword #:data #:accessor .data))

(define (client-monitor client)
  ((@@ (gwwm) client-monitor) client))
(define (client=? c1 c2)
  ((@@ (gwwm) client=?) c1 c2))
(define (client-get-appid client)
  ((@@ (gwwm) client-get-appid) client))
(define (client-list)
  ((@@ (gwwm) client-list)))

(define (current-client)
  ((@@ (gwwm) current-client)))

(define (client-is-floating? client)
  ((@@ (gwwm) client-is-floating?) client))

(define (client-live? client)
  "return #t if client is live, or #f not live."
  (->bool
   (find
    (lambda (c) (client=? client c))
    (client-list))))

(define (client-set-floating! client floating?)
  ((@@ (gwwm) client-set-floating!) client floating? ))

(define (client-is-fullscreen? client)
  ((@@ (gwwm) client-is-fullscreen?) client))

(define (client-set-fullscreen! client fullscreen?)
  ((@@ (gwwm) client-set-fullscreen!) client fullscreen? ))

(define (client-toggle-fullscreen client)
  (set! (client-fullscreen? client)
        (not (client-fullscreen? client))))

(define client-floating?
  (make-procedure-with-setter
   client-is-floating?
   client-set-floating!))

(define client-fullscreen?
  (make-procedure-with-setter
   client-is-fullscreen?
   client-set-fullscreen!))
