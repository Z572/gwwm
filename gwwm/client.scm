(define-module (gwwm client)
  #:autoload (gwwm) (fullscreen-layer float-layer tile-layer overlay-layer top-layer bottom-layer background-layer gwwm-seat)
  #:autoload (gwwm commands) (arrange)
  #:autoload (gwwm config) (gwwm-borderpx g-config config-fullscreenbg)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (wlroots types scene)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types layer-shell)
  #:use-module (ice-9 q)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-71)
  #:use-module (gwwm utils)
  #:use-module (util572 color)
  #:use-module (wlroots xwayland)
  #:use-module (gwwm monitor)
  #:use-module (gwwm hooks)
  #:use-module (wayland listener)
  #:use-module (wayland list)
  #:use-module (wlroots types seat)
  #:use-module (wlroots util box)
  #:use-module (util572 box)
  #:use-module ((system foreign) #:select (pointer->scm null-pointer?))
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types cursor)
  #:use-module (gwwm listener)
  #:use-module (gwwm i18n)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-17)
  #:use-module (gwwm utils srfi-215)
  #:use-module (oop goops)
  #:use-module (oop goops describe)

  #:export (visibleon
            current-client
            client-get-geometry
            client-floating?
            client-fullscreen?
            client-urgent?
            client-list
            client-get-appid
            client-get-title
            client-alive?
            client-at
            client=?
            client-get-parent
            client-is-x11?
            client-is-unmanaged?
            client-is-float-type?
            client-send-close
            client-set-tiled
            client-resize
            client-set-resizing!
            client-title
            client-appid
            client-scene
            client-set-scene!
            client-monitor
            client-border-width
            client-get-size-hints
            client-tags
            client-surface
            client-super-surface
            client-geom
            client-prev-geom
            client-mapped?
            client-wants-fullscreen?
            client-do-set-fullscreen
            client-do-set-floating
            client-scene-surface
            client-resize-configure-serial
            client-init-border
            client-set-border-color
            client-restack-surface
            client-from-wlr-surface
            super-surface->client

            client-set-title-notify

            %fstack
            %clients
            %layer-clients
            <gwwm-client>
            <gwwm-x-client>
            <gwwm-xdg-client>
            <gwwm-layer-client>))

(define-once super-surface->client (make-object-property))

(define %layer-clients
  (make-parameter
   (make-q)
   (lambda (o)
     (if (q? o) o
         (error "not a q! ~A" o)))))
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
  (and m
       (equal? (client-monitor c) m)
       (= (client-tags c)
          (list-ref (slot-ref m 'tagset)
                    (slot-ref m 'seltags)))))

(define-class <gwwm-base-client> ()
  (geom #:accessor client-geom)
  (monitor #:init-value #f
           #:accessor client-monitor
           #:init-keyword #:monitor)
  (super-surface #:init-value #f
                 #:accessor client-super-surface
                 #:init-keyword #:super-surface)
  (surface
   #:allocation #:virtual
   #:slot-ref (lambda (obj) (.surface (client-super-surface obj)))
   #:slot-set! (const #f)
   #:getter client-surface)
  (scene #:init-value #f
         #:accessor client-scene
         #:setter client-set-scene!
         #:init-keyword #:scene)
  (alive? #:init-value #t #:accessor client-alive?))

(define-class <gwwm-client> (<gwwm-base-client>)
  (appid #:accessor client-appid)
  (floating? #:init-value #f
             #:accessor client-floating?)
  (fullscreen? #:init-value #f
               #:accessor client-fullscreen?)
  (fullscreen-bg #:init-value #f
                 #:accessor client-fullscreen-bg)
  (urgent? #:init-value #f
           #:accessor client-urgent?)
  (title #:accessor client-title)
  (tags #:init-value 0 #:accessor client-tags)
  (border-width #:init-value 1 #:accessor client-border-width)
  (prev-geom #:accessor client-prev-geom)
  (scene-surface #:accessor client-scene-surface #:init-value #f)
  (resize-configure-serial #:accessor client-resize-configure-serial #:init-value #f))

(define client-borders (make-object-property))
(define-method (client-set-border-color (c <gwwm-client>) (color <rgba-color>))
  (for-each (lambda (b) (wlr-scene-rect-set-color b color))
            (client-borders c)))
(define-class <gwwm-layer-client> (<gwwm-base-client>))

(define-class <gwwm-x-client> (<gwwm-client>))
(define-class <gwwm-xdg-client> (<gwwm-client>))

(define-method (client-mapped? (c <gwwm-xdg-client>))
  (.mapped (client-super-surface c)))
(define-method (client-mapped? (c <gwwm-x-client>))
  (wlr-xwayland-surface-mapped? (client-super-surface c)))

(define-method (client-init-border (c <gwwm-client>))
  (send-log DEBUG (G_ "client init border") 'c c)
  (define scene (client-scene c))
  (define (create)
    (let ((rect (wlr-scene-rect-create scene 0 0 (make-rgba-color 0 0 0 0))))
      rect))
  (set! (client-borders c)  (list (create) (create) (create) (create)))
  (client-set-tiled c (list 1 2 4 8))
  (modify-instance* (client-geom c)
    (width (+ width (* 2 (gwwm-borderpx))))
    (height (+ height (* 2 (gwwm-borderpx))))))

(define-method (describe (c <gwwm-base-client>))
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

(define-method (client-wants-fullscreen? (c <gwwm-xdg-client>))
  (.fullscreen (.requested (wlr-xdg-surface-toplevel (client-super-surface c)))))

(define-method (client-wants-fullscreen? (c <gwwm-x-client>))
  (.fullscreen (client-super-surface c)))

(define (client-set-fullscreen-bg c)
  (let ((full? (client-fullscreen? c)))
    (if full?
        (unless (client-fullscreen-bg c)
          (let ((bg (wlr-scene-rect-create (client-scene c)
                                           (box-width (client-geom c))
                                           (box-height (client-geom c))
                                           (config-fullscreenbg (g-config)))))
            (set! (client-fullscreen-bg c) bg)
            (wlr-scene-node-lower-to-bottom (.node bg))))

        (when (client-fullscreen-bg c)
          (let ((bg (client-fullscreen-bg c)))
            (wlr-scene-node-destroy (.node bg))
            (set! (client-fullscreen-bg c) #f))))))

(define-method (client-do-set-fullscreen (c <gwwm-client>))
  (client-do-set-fullscreen c (client-fullscreen? c)))

(define-method (client-do-set-fullscreen (c <gwwm-client>) fullscreen?)
  (let ((fullscreen? (->bool fullscreen?)))
    (set! (client-fullscreen? c) fullscreen?)
    (set! (client-border-width c) (if fullscreen? 0 (gwwm-borderpx)))
    (if fullscreen?
        (begin (set! (client-prev-geom c)
                     (shallow-clone (client-geom c)))
               (wlr-scene-node-reparent (client-scene c) fullscreen-layer)
               (client-resize
                c
                (shallow-clone
                 (monitor-area (client-monitor c))) #f))
        (begin (wlr-scene-node-reparent (client-scene c) tile-layer)
               (client-resize c (shallow-clone (client-prev-geom c)))))
    (client-set-fullscreen-bg c)
    (arrange (client-monitor c))))

(define-method (client-do-set-fullscreen (client <gwwm-xdg-client>) fullscreen?)
  (next-method)
  (wlr-xdg-toplevel-set-fullscreen (client-super-surface client) fullscreen?))

(define-method (client-do-set-fullscreen (client <gwwm-x-client>) fullscreen?)
  (next-method)
  (wlr-xwayland-surface-set-fullscreen (client-super-surface client)
                                       fullscreen?))

(define-method (client-do-set-floating (c <gwwm-client>) floating?)
  (when (client-fullscreen? c)
    (client-do-set-fullscreen c #f))
  (set! (client-floating? c) floating?)
  (wlr-scene-node-reparent (client-scene c)
                           (if (client-floating? c) float-layer tile-layer))
  (arrange (client-monitor c)))

(define-method (write (client <gwwm-client>) port)
  (format port "#<~s ~a>"
          (class-name (class-of client))
          (if (client-alive? client)
              (client-get-appid client)
              "*deaded*")))

(define-method (client-get-parent (c <gwwm-xdg-client>))
  (and=> (.parent (wlr-xdg-surface-toplevel
                   (client-super-surface c)))
         (lambda (x) (client-from-wlr-surface (.surface x)))))

(define-method (client-get-parent (c <gwwm-x-client>))
  (and=> (.parent (client-super-surface c))
         (lambda (x) (client-from-wlr-surface (.surface x)))))

(define-method (client-get-appid (c <gwwm-xdg-client>))
  (or (and=> (client-super-surface c)
             (lambda (o)
               (.app-id
                (wlr-xdg-surface-toplevel
                 o))))
      "*unknow*"))

(define-method (client-get-appid (c <gwwm-x-client>))
  (or (and=> (client-super-surface c)
             wlr-xwayland-surface-class)
      "*unknow*"))

(define-method (client-get-title (c <gwwm-xdg-client>))
  (.title
   (wlr-xdg-surface-toplevel
    (client-super-surface c))))

(define-method (client-get-title (c <gwwm-x-client>))
  (wlr-xwayland-surface-title
   (client-super-surface c)))

(define-method (client-send-close (c <gwwm-xdg-client>))
  (wlr-xdg-toplevel-send-close (client-super-surface c)))
(define-method (client-send-close (c <gwwm-x-client>))
  (wlr-xwayland-surface-close (client-super-surface c)))

(define (client-is-x11? client)
  (is-a? client <gwwm-x-client>))

(define-method (client-is-unmanaged? client)
  #f)
(define-method (client-is-unmanaged? (client <gwwm-x-client>))
  (wlr-xwayland-surface-override-redirect (client-super-surface client)))

(define* (client-list #:optional (m #f))
  "return all clients. if provide M, return M's visibleoned clients."
  (let ((clients (car (%clients))))
    (if m
        (filter (cut visibleon <> m) clients)
        clients)))

(define (current-client)
  "return current client or #f."
  (and=> (.focused-surface (.keyboard-state (gwwm-seat)))
         client-from-wlr-surface))

(define-method (client-set-resizing! (c <gwwm-xdg-client>) resizing?)
  (wlr-xdg-toplevel-set-resizing (client-super-surface c) resizing?))
(define-method (client-set-resizing! (c <gwwm-x-client>) resizing?)
  *unspecified*)

(define-method (client-at x y)
  (any (lambda (layer)
         (let ((node (wlr-scene-node-at layer x y)))
           (if node
               (let loop ((node node))
                 (or (let ((o (and (not (null-pointer? (.data node)))
                                   (pointer->scm (.data node)))))
                       (and ((negate (cut is-a? <> <gwwm-layer-client>)) o) o))
                     (and=> (.parent node) loop)))
               #f)))
       (list overlay-layer top-layer
             float-layer
             fullscreen-layer
             tile-layer
             bottom-layer background-layer)))

(define-method (client-at (cursor <wlr-cursor>))
  (client-at (.x cursor) (.y cursor)))

(define (%get-size-hints-helper o)
  (define boxs (list (make <wlr-box>)
                     (make <wlr-box>)))
  (and=> o
         (lambda (o)
           (let-slots o (max-width max-height min-width min-height)
             (modify-instance* (first boxs)
               (width max-width)
               (height max-height))
             (modify-instance* (second boxs)
               (width min-width)
               (height min-height)))))
  (unlist boxs))

(define-method (client-get-size-hints (c <gwwm-xdg-client>))
  (%get-size-hints-helper
   (.current
    (wlr-xdg-surface-toplevel
     (client-super-surface c)))))

(define-method (client-get-size-hints (c <gwwm-x-client>))
  (%get-size-hints-helper (.size-hints (client-super-surface c))))

(define-method (client-set-tiled c (edges <list>))
  (client-set-tiled c (apply logior edges)))

(define-method (client-set-tiled (c <gwwm-xdg-client>) (edges <integer>))
  (wlr-xdg-toplevel-set-tiled
   (client-super-surface c)
   edges))

(define-method (client-restack-surface c)
  *unspecified*)
(define-method (client-restack-surface (c <gwwm-x-client>))
  (wlr-xwayland-surface-restack (client-super-surface c) #f 0))

(define-method (client-set-tiled (c <gwwm-x-client>) edges)
  *unspecified*)

(define (client-from-wlr-surface s)
  (if s
      (or (and-let* (((wlr-surface-is-xdg-surface s))
                     (super-surface (wlr-xdg-surface-from-wlr-surface s))
                     ((eq? (.role super-surface) 'WLR_XDG_SURFACE_ROLE_TOPLEVEL)))
            (super-surface->client super-surface))
          (and-let* (((wlr-surface-is-xwayland-surface s))
                     (super-surface (wlr-xwayland-surface-from-wlr-surface s)))
            (super-surface->client super-surface))
          (if (wlr-surface-is-subsurface s)
              (client-from-wlr-surface
               (wlr-surface-get-root-surface s))
              #f))
      #f))

(define-method (client-get-geometry (c <gwwm-xdg-client>))
  (wlr-xdg-surface-get-geometry (client-super-surface c)))
(define-method (client-get-geometry (c <gwwm-x-client>))
  (let ((s (client-super-surface c)))
    (make-wlr-box (wlr-xwayland-surface-x s)
                  (wlr-xwayland-surface-y s)
                  (wlr-xwayland-surface-width s)
                  (wlr-xwayland-surface-height s))))

(define-method (applybounds (c <gwwm-client>) geom)
  (unless (client-fullscreen? c)
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

(define-method (client-set-size! (c <gwwm-xdg-client>) width height)
  (wlr-xdg-toplevel-set-size (client-super-surface c) width height))

(define-method (client-set-size! (c <gwwm-x-client>) width height)
  (wlr-xwayland-surface-configure (client-super-surface c)
                                  (box-x (client-geom c))
                                  (box-y (client-geom c))
                                  width height)
  0)

(define-method (client-resize-border (c <gwwm-client>))
  (let* ((bw (client-border-width c))
         (geom (client-geom c))
         (heigh (box-height geom))
         (width (box-width geom))
         (borders (client-borders c)))
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
          (client-set-size! c
                            (- width (* 2 bw))
                            (- heigh (* 2 bw))))))

(define-method (client-resize (c <gwwm-client>) geo)
  (client-resize c geo #f))

(define-method (client-resize (c <gwwm-client>)
                              (geo <list>)
                              interact?)
  (client-resize c (list->wlr-box geo) interact?))

(define-method (client-set-title-notify (c <gwwm-client>))
  (lambda (listener data)
    (let ((title (client-title c))
          (new (client-get-title c)))
      (set! (client-title c) new)
      (run-hook update-title-hook c title new)
      (send-log DEBUG (G_ "Client change title.")
                'client c
                'old title
                'new (client-title c)))))
