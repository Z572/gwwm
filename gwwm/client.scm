(define-module (gwwm client)
  #:use-module (srfi srfi-1)
  #:use-module (wlroots types scene)
  #:use-module (ice-9 q)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-71)
  #:use-module (gwwm utils)
  #:use-module (util572 color)
  #:use-module (wlroots xwayland)
  #:use-module (gwwm monitor)
  #:use-module (wayland listener)
  #:use-module (wayland list)
  #:use-module (wlroots util box)
  #:use-module (util572 box)
  #:use-module ((system foreign) #:select (pointer-address))
  #:use-module (wlroots types xdg-shell)
  #:use-module (gwwm listener)
  #:use-module (gwwm i18n)
  #:use-module (srfi srfi-26)
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
            client-resize
            client-set-resizing!
            client-title
            client-scene
            client-set-scene!
            client-monitor
            client-border-width
            client-set-border-width!
            client-get-size-hints
            client-tags
            client-surface
            client-geom
            %fstack
            %clients
            <gwwm-client>
            <gwwm-x-client>
            <gwwm-layer-client>))

(define %fstack
  (make-parameter
   (make-q)
   (lambda (o)
     (if (q? o) o
         (error "not a q! ~A" o)))))
(eval-when (expand load eval)
  (load-extension "libgwwm" "scm_init_gwwm_client"))

(define-method (client-set-border-color c (color <rgba-color>))
  #t)
(define (visibleon c m)
  ((@@ (gwwm) visibleon) c m))

(define-class <gwwm-base-client> (<listener-manager>)
  (data #:init-keyword #:data
        #:accessor .data
        #:class <hidden-slot>)
  (geom #:init-value #f #:accessor client-geom)
  (type #:init-keyword #:type #:getter client-type
        #:setter client-set-type!)
  (monitor #:init-value #f
           #:accessor client-monitor)
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
  (fullscreen-bg #:init-value #f
                 #:accessor client-fullscreen-bg)
  (urgent? #:init-value #f
           #:accessor client-urgent?)
  (title #:allocation #:virtual
         #:slot-ref (lambda (c)
                      (if (client-alive? c)
                          (client-get-title c)
                          "*deaded*"))
         #:slot-set! (lambda _ #t)
         #:getter client-title)
  (tags #:init-value 0 #:accessor client-tags)
  (borders #:init-value (list))
  (border-width #:init-value 1 #:accessor client-border-width)
  (prev-geom #:accessor client-prev-geom #:init-value #f)
  (scene-surface #:accessor client-scene-surface #:init-value #f)
  (resize-configure-serial #:accessor client-resize-configure-serial #:init-value #f))

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
  (make-parameter
   (make-q)
   (lambda (o)
     (if (q? o) o
         (error "not a q! ~A" o)))))

(define-method (client-do-set-fullscreen (c <gwwm-client>))
  (client-do-set-fullscreen c (client-fullscreen? c)))
(define-method (client-do-set-fullscreen (client <gwwm-client>) fullscreen?)
  (wlr-xdg-toplevel-set-fullscreen (client-xdg-surface client) fullscreen?))

(define (client-xwayland-surface c)
  (and=> (client-surface c)
         (cut wlr-xwayland-surface-from-wlr-surface <>)))

(define (client-xdg-surface c)
  (and=> (client-surface c)
         (cut wlr-xdg-surface-from-wlr-surface <>)))

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
  (or (and=> (client-xwayland-surface c)
             wlr-xwayland-surface-class)
      "*unknow*"))

(define-method (client-get-title (c <gwwm-client>))
  (wlr-xdg-toplevel-title
   (wlr-xdg-surface-toplevel
    (client-xdg-surface c))))
(define-method (client-get-title (c <gwwm-x-client>))
  (wlr-xwayland-surface-title
   (client-xwayland-surface c)))

(define-method (logout-client (c <gwwm-base-client>))
  (remove-listeners c))
(define-method (logout-client (c <gwwm-client>))
  (next-method)
  (set! (.data c) 0))
(define-method (logout-client (c <gwwm-layer-client>))
  (next-method)
  (set! (.data c) 0))

(define-method (client-send-close (c <gwwm-client>))
  (wlr-xdg-toplevel-send-close (client-xdg-surface c)))
(define-method (client-send-close (c <gwwm-x-client>))
  (wlr-xwayland-surface-close (client-xwayland-surface c)))

(define-method (equal? (o1 <gwwm-client>)
                       (o2 <gwwm-client>))
  (client=? o1 o2))

(define-method (client=? (c1 <gwwm-client>) (c2 <gwwm-client>))
  (equal? (.data c1) (.data c2)))

(define (client-is-x11? client)
  (->bool (member (client-type client) '("X11Managed" "X11Unmanaged"))))

(define (client-is-unmanaged? client)
  (string= (client-type client) "X11Unmanaged"))

(define (client-list)
  "return all clients."
  (car (%clients)))

(define (current-client)
  "return current client or #f."
  (if (q-empty? (%fstack))
      #f
      (let ((c (q-front (%fstack))))
        (if (visibleon c (current-monitor))
            c
            #f))))

(define-method (client-set-resizing! (c <gwwm-client>) resizing?)
  (wlr-xdg-toplevel-set-resizing (client-xdg-surface c) resizing?))
(define-method (client-set-resizing! (c <gwwm-x-client>) resizing?)
  *unspecified*)

(define (client-alive? client)
  "return #t if client is alive, or #f deaded."
  (not (zero? (.data client))))

(define-method (client-set-tiled c (edges <list>))
  (client-set-tiled c (apply logior edges)))

(define-method (client-set-tiled (c <gwwm-client>) (edges <integer>))
  (wlr-xdg-toplevel-set-tiled
   (client-xdg-surface c)
   edges))

(define-method (client-set-tiled (c <gwwm-x-client>) edges)
  *unspecified*)

(define-method (applybounds (c <gwwm-client>) geom)
  (unless (client-is-fullscreen? c)
    (let ((_ min* (client-get-size-hints c))
          (bw (client-border-width c))
          (box (client-geom c)))
      (modify-instance* box
        (width (max width (+ (box-width min*) (* 2 bw)) ))
        (height (max height (+ (box-height min*) (* 2 bw)) )))

      (let-slots geom ((x xx) (y yy) (height hh) (width ww))
        (let-slots box (x y height width)
          (if (>= x (+ xx ww))
              (set! (box-x box) (- (+ xx ww) width)))
          (if (>= y (+ yy hh))
              (set! (box-y box) (- (+ yy hh) height)))
          (if (<= (+ x width (* 2 bw)) xx)
              (set! (box-x box) xx))
          (if (<= (+ y height (* 2 bw)) y)
              (set! (box-x box) yy)))))))

(define-method (client-resize-border (c <gwwm-client>))
  (let* ((bw (client-border-width c))
         (geom (client-geom c))
         (heigh (box-height geom))
         (width (box-width geom))
         (borders (slot-ref c 'borders)))
    (wlr-scene-rect-set-size (list-ref borders 0) width bw)
    (wlr-scene-rect-set-size (list-ref borders 1) width bw)
    (wlr-scene-rect-set-size (list-ref borders 2) bw (- heigh (* 2 bw)))
    (wlr-scene-rect-set-size (list-ref borders 3) bw (- heigh (* 2 bw)))
    (wlr-scene-node-set-position (.node (list-ref borders 1)) 0 (- heigh bw ) )
    (wlr-scene-node-set-position (.node (list-ref borders 2)) 0 bw )
    (wlr-scene-node-set-position (.node (list-ref borders 3)) (- width bw) bw )))

(define-method (client-resize (c <gwwm-client>) geo (interact? <boolean>))
  (set! (client-geom c) geo)
  (applybounds
   c
   (if interact?
       ((@ (gwwm) entire-layout-box))
       (monitor-window-area (client-monitor c))))
  (let* ((bw (client-border-width c))
         (geom (client-geom c))
         (heigh (box-height geom))
         (width (box-width geom)))
    (wlr-scene-node-set-position (client-scene c) (box-x geo) (box-y geo))
    (wlr-scene-node-set-position (client-scene-surface c) bw bw)
    (client-resize-border c)
    (set! (client-resize-configure-serial c)
          (client-set-size c
                           (- width (* 2 bw))
                           (- heigh (* 2 bw))))))

(define-method (client-resize (c <gwwm-client>) geo)
  (client-resize c geo #f))

(define-method (client-resize (c <gwwm-client>)
                              (geo <list>)
                              interact?)
  (client-resize c (list->wlr-box geo) interact?))
