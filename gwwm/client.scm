(define-module (gwwm client)
  #:autoload (gwwm) (fullscreen-layer float-layer tile-layer overlay-layer top-layer bottom-layer background-layer  gwwm-seat arrangelayers)
  #:autoload (gwwm commands) (arrange)
  #:use-module (gwwm config)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-189)
  #:use-module (gwwm utils srfi-215)
  #:use-module (wlroots types scene)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types subcompositor)
  #:use-module (wlroots types layer-shell)
  #:use-module (wlroots time)
  #:use-module (ice-9 q)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
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
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types cursor)
  #:use-module (gwwm listener)
  #:use-module (gwwm i18n)
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
            client-resize-configure-serial
            client-init-border
            client-set-border-color
            client-restack-surface
            client-from-wlr-surface
            super-surface->client
            super-surface->scene
            scene-node->client
            client-set-title-notify
            client-commit-notify
            client-destroy-notify
            client-request-fullscreen-notify
            surface->scene
            client-scene-set-enabled
            client-scene-move
            client-scene-move/relatively
            client-scene-raise-to-top
            %fstack
            %clients
            %layer-clients
            <gwwm-base-client>
            <gwwm-client>
            <gwwm-xdg-client>
            <gwwm-layer-client>))

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
(define-once visibleon-functions
  (list
   (lambda (c m)
     (equal? (client-tags c)
             (list-ref (slot-ref m 'tagset)
                       (slot-ref m 'seltags))))))
(define (visibleon c m)
  (and m
       (equal? (client-monitor c) m)
       (and-map (lambda (v) (v c m))
                visibleon-functions)))

(define-class <gwwm-base-client> ()
  (geom #:accessor client-geom
        #:init-thunk (lambda ()
                       (make <wlr-box>)))
  (monitor #:init-value #f
           #:accessor client-monitor
           #:init-keyword #:monitor)
  (super-surface #:init-value #f
                 #:accessor client-super-surface
                 #:init-keyword #:super-surface)
  (surface
   #:allocation #:virtual
   #:slot-ref (lambda (obj) (.surface (slot-ref obj 'super-surface)))
   #:slot-set! (const #f)
   #:getter client-surface)
  (scene #:init-value #f
         #:accessor client-scene
         #:setter client-set-scene!
         #:init-keyword #:scene)
  (alive? #:init-value #t #:accessor client-alive?)
  #:metaclass <redefinable-class>)

(define-class <gwwm-client> (<gwwm-base-client>)
  (appid #:accessor client-appid)
  (floating? #:init-value #f
             #:accessor client-floating?)
  (fullscreen? #:init-value #f
               #:accessor client-fullscreen?)
  (urgent? #:init-value #f
           #:accessor client-urgent?)
  (title #:accessor client-title)
  (tags #:init-value 0 #:accessor client-tags)
  (border-width #:init-value 1 #:accessor client-border-width)
  (prev-geom #:accessor client-prev-geom)
  (resize-configure-serial #:accessor client-resize-configure-serial #:init-value #f))



(define-once super-surface->client (make-object-property))
(define-once super-surface->scene (make-object-property))
(let-syntax ((def
              (lambda (x)
                (syntax-case x ()
                  ((_ n %n f s)
                   #'(begin
                       (define-once %n (make-hash-table 500))
                       (define-method (n (o f))
                         (hashq-ref %n o))
                       (define-method ((setter n) (o f) (o2 s))
                         (hashq-set! %n o o2))
                       (export n)))))))
  (def surface->scene %surface->scene
       <wlr-surface> <wlr-scene-tree>)
  (def scene-node->client %scene-node->client
       <wlr-scene-node> <gwwm-base-client>))

(define-once client-borders (make-object-property))
(define-method (client-set-border-color (c <gwwm-client>) (color <rgba-color>))
  (for-each (lambda (b) (wlr-scene-rect-set-color b color))
            (or (client-borders c) '())))
(define-class <gwwm-layer-client> (<gwwm-base-client>)
  (scene-layer-surface #:init-keyword #:scene-layer-surface))

(define-class <gwwm-xdg-client> (<gwwm-client>))

(define-method (initialize (c <gwwm-base-client>) initargs)
  (next-method)
  (set! (super-surface->client (client-super-surface c)) c))
(define-method (initialize (c <gwwm-client>) initargs)
  (next-method)
  (set! (client-appid c) (client-get-appid c))
  (set! (client-title c) (client-get-title c)))

(define-method (initialize (c <gwwm-layer-client>) initargs)
  (next-method)
  (q-push! (%layer-clients) c))

(define-method (client-mapped? (c <gwwm-xdg-client>))
  (.mapped (client-super-surface c)))

(define-method (client-init-border (c <gwwm-client>))
  (send-log DEBUG (G_ "client init border") 'c c)
  (define scene (client-scene c))
  (define (create)
    (let ((rect (wlr-scene-rect-create scene 0 0 (make-rgba-color 0 0 0 0))))
      rect))
  (set! (client-borders c)  (list (create) (create) (create) (create)))
  (client-set-tiled c (list 1 2 4 8))
  (modify-instance* (client-geom c)
                    (width (+ width (* 2 (borderpx))))
                    (height (+ height (* 2 (borderpx))))))

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

(define-method (client-do-set-fullscreen (c <gwwm-client>))
  (client-do-set-fullscreen c (client-fullscreen? c)))

(define-method (client-do-set-fullscreen (c <gwwm-client>) fullscreen?)
  (send-log DEBUG (G_ "client do fullscreen") 'client c 'fullscreen? fullscreen?)
  (let ((fullscreen? (->bool fullscreen?)))
    (set! (client-fullscreen? c) fullscreen?)
    (set! (client-border-width c) (if fullscreen? 0 (borderpx)))
    (if fullscreen?
        (begin (set! (client-prev-geom c)
                     (shallow-clone (client-geom c)))
               (client-resize
                c
                (shallow-clone
                 (monitor-area (client-monitor c))) #f))
        (client-resize c (shallow-clone (client-prev-geom c))))
    (run-hook client-fullscreen-hook c fullscreen?)
    (arrange (client-monitor c))))

(define-method (client-do-set-fullscreen (client <gwwm-xdg-client>) fullscreen?)
  (next-method)
  (wlr-xdg-toplevel-set-fullscreen
   (wlr-xdg-surface-toplevel (client-super-surface client)) fullscreen?))

(define-method (client-do-set-floating (c <gwwm-client>) floating?)
  (send-log DEBUG (G_ "client do floating") 'client c )
  (when (client-fullscreen? c)
    (client-do-set-fullscreen c #f))
  (set! (client-floating? c) floating?)
  (arrange (client-monitor c)))

(define-method (write (client <gwwm-client>) port)
  (format port "#<~s ~a ~x>"
          (class-name (class-of client))
          (if (client-alive? client)
              (client-get-appid client)
              "*deaded*")
          (object-address client)))

(define-method (client-get-parent (c <gwwm-xdg-client>))
  (and=> (.parent (wlr-xdg-surface-toplevel
                   (client-super-surface c)))
         (lambda (x) (client-from-wlr-surface (.surface x)))))

(define-method (client-get-appid (c <gwwm-xdg-client>))
  (or (and=> (client-super-surface c)
             (lambda (o)
               (.app-id
                (wlr-xdg-surface-toplevel
                 o))))
      "*unknow*"))

(define-method (client-get-title (c <gwwm-xdg-client>))
  (.title
   (wlr-xdg-surface-toplevel
    (client-super-surface c))))

(define-method (client-send-close (c <gwwm-xdg-client>))
  (wlr-xdg-toplevel-send-close (wlr-xdg-surface-toplevel(client-super-surface c))))

(define-method (client-is-unmanaged? client)
  #f)

(define* (client-list #:optional (m #f))
  "return all clients. if provide M, return M's clients."
  (let ((clients (car (%clients))))
    (if m
        (filter (lambda (c) (eq? (client-monitor c) m)) clients)
        clients)))

(define (current-client)
  "return current client or #f."
  (and=> (.focused-surface (.keyboard-state (gwwm-seat)))
         client-from-wlr-surface))

(define-method (client-set-resizing! (c <gwwm-xdg-client>) resizing?)
  (wlr-xdg-toplevel-set-resizing (wlr-xdg-surface-toplevel (client-super-surface c)) resizing?))

(define-method (client-at x y)
  (or (any (lambda (layer)
             (let* ((node p (wlr-scene-node-at (.node layer) x y)))
               (if node
                   (let loop ((node* node))
                     (or (let ((o (scene-node->client node*)))
                           (and o
                                (just o (and=> (wlr-scene-object-from-node node)
                                               (lambda (o)
                                                 (and (wlr-scene-surface? o)
                                                      (.surface o))))
                                      (car p) (cdr p))))
                         (and=> (and=> (.parent node*) .node)
                                loop)))
                   #f)))
           (list overlay-layer top-layer
                 float-layer
                 fullscreen-layer
                 tile-layer
                 bottom-layer background-layer))
      (nothing)))

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


(define-method (client-set-tiled c (edges <list>))
  (client-set-tiled c (apply logior edges)))

(define-method (client-set-tiled (c <gwwm-xdg-client>) (edges <integer>))
  (wlr-xdg-toplevel-set-tiled
   (wlr-xdg-surface-toplevel (client-super-surface c))
   edges))

(define-method (client-restack-surface c)
  *unspecified*)

(define (client-from-wlr-surface s)
  (and s
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
               #f))))

(define-method (client-get-geometry (c <gwwm-xdg-client>))
  (wlr-xdg-surface-get-geometry (client-super-surface c)))

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
  (wlr-xdg-toplevel-set-size (wlr-xdg-surface-toplevel (client-super-surface c)) width height))


(define-method (client-resize-border (c <gwwm-client>))
  (and-let* ((borders (client-borders c))
             ((not (null-list? borders)))
             (bw (client-border-width c))
             (geom (client-geom c))
             (heigh (box-height geom))
             (width (box-width geom)))

    (wlr-scene-rect-set-size (list-ref borders 0) width bw)
    (wlr-scene-node-set-position (.node (list-ref borders 0)) (- bw) (- bw))

    (wlr-scene-rect-set-size (list-ref borders 1) width bw)
    (wlr-scene-node-set-position (.node (list-ref borders 1)) (- bw) (- heigh bw bw ))

    (wlr-scene-rect-set-size (list-ref borders 2) bw heigh)
    (wlr-scene-node-set-position (.node (list-ref borders 2)) (- bw) (- bw ) )

    (wlr-scene-rect-set-size (list-ref borders 3) bw heigh)
    (wlr-scene-node-set-position (.node (list-ref borders 3)) (- width bw bw) (- bw))))

(define-method (client-scene-raise-to-top (c <gwwm-base-client>))
  (wlr-scene-node-raise-to-top (.node (client-scene c))))

(define-method (client-scene-set-enabled (c <gwwm-base-client>) n)
  (wlr-scene-node-set-enabled (.node (client-scene c)) n))
(define-method (client-scene-move (c <gwwm-base-client>) x y)
  (wlr-scene-node-set-position (.node (client-scene c)) x y))
(define-method (client-scene-move (c <gwwm-client>) x y)
  (let ((bw (client-border-width c)))
    (next-method c (+ bw x) (+ bw y))))
(define-method (client-scene-move/relatively (c <gwwm-base-client>) x y)
  (let ((node (.node (client-scene c))))
    (wlr-scene-node-set-position
     (.node (client-scene c))
     (+ (.x node) x)
     (+ (.y node) y))))

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


    (set! (client-resize-configure-serial c)
          (client-set-size! c
                            (- width (* 2 bw))
                            (- heigh (* 2 bw))))
    (client-resize-border c)
    (client-scene-move c (box-x geo) (box-y geo))))

(define-method (client-resize (c <gwwm-client>) geo)
  (client-resize c geo #f))

(define-method (client-destroy-notify (c <gwwm-base-client>))
  (lambda (listener data)
    (send-log DEBUG "client destroy" 'client c)
    (run-hook client-destroy-hook c)
    ;; mark it destroyed.
    (set! (client-alive? c) #f)))

(define-method (client-destroy-notify (c <gwwm-client>))
  (let ((next (next-method c)))
    (lambda (listener data)
      (next listener data)
      (set! (client-scene c) #f))))

(define-method (client-destroy-notify (c <gwwm-layer-client>))
  (let ((next (next-method c)))
    (lambda (listener data)
      (q-remove! (%layer-clients) c)
      (for-each (cut q-remove! <> c)
                (slot-ref (client-monitor c) 'layers))
      (next listener data)
      (and=> (client-monitor c) arrangelayers))))


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

(define-method (client-request-fullscreen-notify (c <gwwm-client>))
  (lambda (listener data)
    (send-log DEBUG "client request fullscreen" 'client c)
    (let ((fullscreen? (client-wants-fullscreen? c))
          (event (wrap-wlr-xdg-toplevel-set-fullscreen-event data)))
      (if (client-monitor c)
          (client-do-set-fullscreen c fullscreen?)
          (set! (client-fullscreen? c) fullscreen?))
      (run-hook fullscreen-event-hook c event))))


(define-method (client-commit-notify (c <gwwm-client>))
  (lambda (a b)
    ;;(send-log DEBUG "client commit" 'client c)
    (let ((box (client-get-geometry c))
          (m (client-monitor c)))
      (when (and m (not (box-empty? box))
                 (or (not (= (box-width box)
                             (- (box-width (client-geom c))
                                (* 2 (client-border-width c)))))
                     (not (= (box-height box)
                             (- (box-height (client-geom c))
                                (* 2 (client-border-width c)))))))
        (arrange m)))
    (run-hook surface-commit-event-hook c)))

(define-method (client-commit-notify (c <gwwm-xdg-client>))
  (let ((next (next-method c)))
    (lambda (listener data)
      (next listener data)
      (let ((serial (client-resize-configure-serial c))
            (current (.current (client-super-surface c)))
            (pending (.pending (client-super-surface c))))
        (when (and (not (zero? serial))
                   (or (<= serial (.configure-serial current))
                       (and (= (box-width (.geometry current))
                               (box-width (.geometry pending)))
                            (= (box-height (.geometry current))
                               (box-height (.geometry pending))))))
          (set! (client-resize-configure-serial c) 0))))))
