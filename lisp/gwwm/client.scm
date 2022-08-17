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
            client-alive?
            client=?
            client-get-parent
            client-is-x11?
            client-type
            <gwwm-client>))

(define-class <gwwm-client> ()
  (data #:init-keyword #:data #:accessor .data))

(define-method (write (client <gwwm-client>) port)
  (format port "#<<gwwm-client ~a>"
          (if (client-alive? client)
              (client-get-appid client)
              "*deaded*"))
  )
(define (client-monitor client)
  ((@@ (gwwm) client-monitor) client))

(define (client-get-parent client)
  ((@@ (gwwm) client-get-parent) client))

(define-method (equal? (o1 <gwwm-client>)
                       (o2 <gwwm-client>))
  (client=? o1 o2))

(define-method (client=? (c1 <gwwm-client>) (c2 <gwwm-client>))
  (equal? (.data c1) (.data c2)))

(define (client-xdg-surface client)
  ((@@ (gwwm) client-xdg-surface) client))

(define (client-type client)
  ((@@ (gwwm) client-type) client))
(define (client-is-x11? client)
  (->bool (member (client-type client) '("X11Managed" "X11Unmanaged"))))

(define (client-get-appid client)
  ((@@ (gwwm) client-get-appid) client))

(define (client-list)
  ((@@ (gwwm) client-list)))

(define (current-client)
  ((@@ (gwwm) current-client)))

(define (client-is-floating? client)
  ((@@ (gwwm) client-is-floating?) client))

(define (client-alive? client)
  "return #t if client is alive, or #f deaded."
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
