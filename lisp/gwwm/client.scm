(define-module (gwwm client)
  #:export (current-client
            client-floating?
            client-set-floating
            client-is-floating?))

(define (current-client)
  ((@@ (gwwm) current-client)))

(define (client-is-floating? client)
  ((@@ (gwwm) client-is-floating?) client))

(define (client-set-floating client floating?)
  ((@@ (gwwm) client-set-floating) client floating? ))

(define client-floating?
  (make-procedure-with-setter
   client-is-floating?
   client-set-floating))
