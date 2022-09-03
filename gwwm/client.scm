(define-module (gwwm client)
  #:use-module (srfi srfi-1)
  #:use-module (wlroots xwayland)
  #:use-module (wlroots util box)
  #:use-module (wlroots types xdg-shell)
  #:use-module (srfi srfi-17)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:export (current-client
            client-floating?
            client-set-floating!
            client-is-floating?
            client-fullscreen?
            client-is-fullscreen?
            client-set-fullscreen!
            client-urgent?
            client-is-urgent?
            client-set-urgent!
            client-list
            client-get-appid
            client-get-title
            client-alive?
            client=?
            client-get-parent
            client-is-x11?
            client-type
            client-send-close
            client-set-tiled
            client-xwayland-surface
            client-xdg-surface
            client-resize
            client-set-resizing!
            client-title
            <gwwm-client>))

(define (client-type client)
  ((@@ (gwwm) client-type) client))
(define-class <gwwm-client> ()
  (data #:init-keyword #:data #:accessor .data)
  (type #:allocation #:virtual
        #:slot-ref (lambda (c)
                     (if (client-alive? c)
                         (client-type c)
                         "*deaded*"))
        #:slot-set! (lambda _ #t))
  (fullscreen? #:init-value #f
               #:accessor client-fullscreen?)
  (urgent? #:init-value #f
           #:accessor client-urgent?)
  (title #:allocation #:virtual
         #:slot-ref (lambda (c)
                      (if (client-alive? c)
                          (client-get-title c)
                          "*deaded*"))
         #:slot-set! (lambda _ #t)
         #:getter client-title))

(define client-is-fullscreen? client-fullscreen?)

(define-method (describe (c <gwwm-client>))
  (if (client-alive? c)
      (next-method)
      (begin (format #t "~S is a *deaded* client.~%" c)
             *unspecified*)))

(define-once %clients
  (make-hash-table))

(define client-set-fullscreen! (setter client-fullscreen?))
(define client-is-urgent? client-urgent?)
(define client-set-urgent! (setter client-urgent?))

(define-method (write (client <gwwm-client>) port)
  (format port "#<<gwwm-client ~a>"
          (if (client-alive? client)
              (client-get-appid client)
              "*deaded*")))
(define (client-get-appid c)
  (if (client-is-x11? c)
      (wlr-xwayland-surface-class
       (client-xwayland-surface c))
      (wlr-xdg-toplevel-appid
       (wlr-xdg-surface-toplevel
        (client-xdg-surface c)))))

(define (client-get-title c)
  (if (client-is-x11? c)
      (wlr-xwayland-surface-title
       (client-xwayland-surface c))
      (wlr-xdg-toplevel-title
       (wlr-xdg-surface-toplevel
        (client-xdg-surface c)))))

(define (client-xwayland-surface client)
  ((@@ (gwwm) client-xwayland-surface) client))

(define (client-send-close client)
  (if (client-is-x11? client)
      (wlr-xwayland-surface-close (client-xwayland-surface client))
      (wlr-xdg-toplevel-send-close (client-xdg-surface client))))
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


(define (client-is-x11? client)
  (->bool (member (client-type client) '("X11Managed" "X11Unmanaged"))))

(define (client-list)
  (hash-map->list (lambda (_ b) b) %clients))

(define (current-client)
  ((@@ (gwwm) current-client)))

(define (client-is-floating? client)
  ((@@ (gwwm) client-is-floating?) client))

(define (client-set-resizing! client resizing?)
  (unless (client-is-x11? client)
    (wlr-xdg-toplevel-set-resizing (client-xdg-surface client) resizing?)))

(define (client-alive? client)
  "return #t if client is alive, or #f deaded."
  (->bool
   (find
    (lambda (c) (client=? client c))
    (client-list))))

(define-method (client-set-tiled c (edges <list>))
  (client-set-tiled c (apply logior edges)))

(define-method (client-set-tiled c (edges <integer>))
  (unless (client-is-x11? c)
    (wlr-xdg-toplevel-set-tiled
     (client-xdg-surface c)
     edges)))
(define (client-set-floating! client floating?)
  ((@@ (gwwm) client-set-floating!) client floating? ))

(define client-floating?
  (make-procedure-with-setter
   client-is-floating?
   client-set-floating!))

(define-method (client-resize (c <gwwm-client>) geo (interact? <boolean>))
  ((@@ (gwwm) %resize) c geo interact?))

(define-method (client-resize (c <gwwm-client>) geo)
  (client-resize c geo #f))

(define-method (client-resize (c <gwwm-client>)
                              (geo <list>)
                              interact?)
  (client-resize c (list->wlr-box geo) interact?))
