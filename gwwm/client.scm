(define-module (gwwm client)
  #:use-module (srfi srfi-1)
  #:use-module (wlroots types scene)
  #:use-module (ice-9 control)
  #:use-module (util572 color)
  #:use-module (wlroots xwayland)
  #:use-module (wlroots util box)
  #:use-module ((system foreign) #:select (pointer-address))
  #:use-module (wlroots types xdg-shell)
  #:use-module (gwwm i18n)
  #:use-module (srfi srfi-17)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:export (visibleon
            current-client
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
            client-is-unmanaged?
            client-type
            client-is-float-type?
            client-send-close
            client-set-tiled
            client-xwayland-surface
            client-xdg-surface
            client-resize
            client-set-resizing!
            client-title
            client-scene
            client-set-scene!
            client-monitor
            client-border-width
            client-set-border-width!
            client-get-size-hints
            client-surface
            <gwwm-client>
            <gwwm-x-client>
            <gwwm-layer-client>))

(define-method (client-set-border-color c (color <rgba-color>))
  #t)
(define (visibleon c m)
  ((@@ (gwwm) visibleon) c m))

(define (client-get-size-hints c)
  ((@@ (gwwm) client-get-size-hints) c))

(define (client-is-float-type? client)
  ((@@ (gwwm) client-is-float-type?) client))

(define-class <gwwm-base-client> ()
  (data #:init-keyword #:data
        #:accessor .data
        #:class <hidden-slot>)
  (type #:init-keyword #:type #:getter client-type
        #:setter client-set-type!)
  (surface #:init-value #f
           #:accessor client-surface)
  (scene #:init-value #f
         #:accessor client-scene
         #:setter client-set-scene!))

(define-class <gwwm-client> (<gwwm-base-client>)
  (appid #:allocation #:virtual
         #:slot-ref (lambda (c)
                      (wlr-xdg-toplevel-appid
                       (wlr-xdg-surface-toplevel
                        (client-xdg-surface c))))
         #:slot-set! (const #f)
         #:getter client-get-appid)
  (floating? #:init-value #f
             #:accessor client-floating?)
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
         #:getter client-title)

  (borders #:init-value (list))
  (border-width #:init-value 1 #:accessor client-border-width))

(define-method (client-set-border-color (c <gwwm-client>) (color <rgba-color>))
  (for-each (lambda (b) (wlr-scene-rect-set-color b color))
            (slot-ref c 'borders)))
(define-class <gwwm-layer-client> (<gwwm-base-client>))

(define-class <gwwm-x-client> (<gwwm-client>))

(define client-is-fullscreen? client-fullscreen?)
(define client-set-border-width! (setter client-border-width))


;; (define (client-set-border-width! c width)
;;   (set! (client-border-width c) (max 0 width)))

(define-method (describe (c <gwwm-client>))
  (if (client-alive? c)
      (next-method)
      (begin (format #t (G_ "~S is a *deaded* client.~%") c)
             *unspecified*)))

(define-once %clients
  (make-hash-table))

(define-once %layer-clients
  (make-hash-table))

(define-method (client-do-set-fullscreen (c <gwwm-client>))
  (client-do-set-fullscreen c (client-fullscreen? c)))
(define-method (client-do-set-fullscreen (client <gwwm-client>) fullscreen?)
  (wlr-xdg-toplevel-set-fullscreen (client-xdg-surface client) fullscreen?))
(define-method (client-do-set-fullscreen (client <gwwm-x-client>) fullscreen?)
  (wlr-xwayland-surface-set-fullscreen (client-xwayland-surface client)
                                       fullscreen?))
;; (define-method (set-fullscreen (c <gwwm-client>) fullscreen?)
;;   (let ((fullscreen? (->bool fullscreen?)))
;;     (set! (client-fullscreen? c) fullscreen?)
;;     (set! (client-border-width c) 1)
;;     (client-do-set-fullscreen c )
;;     (if fullscreen?
;;         (begin (set! client-p)))))
(define client-set-fullscreen! (setter client-fullscreen?))
(define client-is-floating? client-floating?)
(define client-set-floating! (setter client-floating?))
(define client-is-urgent? client-urgent?)
(define client-set-urgent! (setter client-urgent?))

(define-method (write (client <gwwm-client>) port)
  (format port "#<~s ~a>"
          (class-name (class-of client))
          (if (client-alive? client)
              (client-get-appid client)
              "*deaded*")))

(define-method (client-get-appid (c <gwwm-x-client>))
  (wlr-xwayland-surface-class
   (client-xwayland-surface c)))

(define-method (client-get-title (c <gwwm-client>))
  (wlr-xdg-toplevel-title
   (wlr-xdg-surface-toplevel
    (client-xdg-surface c))))
(define-method (client-get-title (c <gwwm-x-client>))
  (wlr-xwayland-surface-title
   (client-xwayland-surface c)))
(define (client-xwayland-surface client)
  ((@@ (gwwm) client-xwayland-surface) client))

(define-method (logout-client (c <gwwm-client>))
  (hashq-remove! %clients (.data c))
  (set! (.data c) 0))
(define-method (logout-client (c <gwwm-layer-client>))
  (hashq-remove! %layer-clients (.data c))
  (set! (.data c) 0))

(define-method (client-send-close (c <gwwm-client>))
  (wlr-xdg-toplevel-send-close (client-xdg-surface c)))
(define-method (client-send-close (c <gwwm-x-client>))
  (wlr-xwayland-surface-close (client-xwayland-surface c)))

(define (client-monitor client)
  ((@@ (gwwm) client-monitor) client))

(define (client-get-parent client)
  "return CLIENT's parent or #f not found."
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

(define (client-is-unmanaged? client)
  (string= (client-type client) "X11Unmanaged"))

(define (client-list)
  "return all clients."
  (hash-map->list (lambda (_ b) b) %clients))

(define (current-client)
  "return current client or #f."
  ((@@ (gwwm) current-client)))

(define-method (client-set-resizing! (c <gwwm-client>) resizing?)
  (wlr-xdg-toplevel-set-resizing (client-xdg-surface c) resizing?))
(define-method (client-set-resizing! (c <gwwm-x-client>) resizing?)
  *unspecified*)

(define (client-alive? client)
  "return #t if client is alive, or #f deaded."
  (let/ec return
    (hash-for-each
     (lambda (_ b)
       (when (equal? b client)
         (return #t)))
     %clients) #f))

(define-method (client-set-tiled c (edges <list>))
  (client-set-tiled c (apply logior edges)))

(define-method (client-set-tiled (c <gwwm-client>) (edges <integer>))
  (wlr-xdg-toplevel-set-tiled
   (client-xdg-surface c)
   edges))

(define-method (client-set-tiled (c <gwwm-x-client>) edges)
  *unspecified*)

(define-method (client-resize (c <gwwm-client>) geo (interact? <boolean>))
  ((@@ (gwwm) %resize) c geo interact?))

(define-method (client-resize (c <gwwm-client>) geo)
  (client-resize c geo #f))

(define-method (client-resize (c <gwwm-client>)
                              (geo <list>)
                              interact?)
  (client-resize c (list->wlr-box geo) interact?))
