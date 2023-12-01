(define-module (gwwm commands)
  #:autoload (gwwm) (cursor-mode gwwm-xcursor-manager
                                 gwwm-session
                                 fullscreen-layer
                                 tile-layer
                                 grabc grabcx
                                 grabcy gwwm-cursor float-layer gwwm-display gwwm-scene)
  #:use-module (oop goops)
  #:use-module (wlroots backend session)
  #:use-module (wlroots backend)
  #:use-module (wlroots types scene)
  #:use-module (wlroots types cursor)
  #:use-module (wlroots types xcursor-manager)
  #:use-module (wayland server display)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-189)
  #:use-module (gwwm utils srfi-215)
  #:use-module (ice-9 control)
  #:use-module (gwwm config)
  #:use-module (util572 box)
  #:use-module (gwwm i18n)
  #:use-module (gwwm utils)
  #:use-module (gwwm monitor)
  #:use-module (gwwm layout)
  #:use-module (gwwm client)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (chvt
            killclient
            togglefullscreen
            togglefloating
            toggletag
            focusclient
            focustop
            gwwm-quit
            setlayout
            arrange
            focusstack
            tag
            moveresize
            incmaster
            view))

(define* (tag int #:optional (client (current-client)))
  (when client
    (set! (client-tags client) int)
    (focusclient (focustop (current-monitor)) #t)
    (arrange (current-monitor))))

(define (view i)
  (send-log DEBUG (G_ "view"))
  (let ((m(current-monitor)))
    (unless (= i (list-ref (slot-ref m 'tagset) (slot-ref m 'seltags)))
      (slot-set! m 'seltags (if (= 1 (slot-ref m 'seltags)) 0 1))
      (list-set! (slot-ref m 'tagset)  (slot-ref m 'seltags) i)
      (focusclient (focustop (current-monitor)) #t)
      (arrange (current-monitor)))))
(define* (toggletag tag #:optional (client (current-client)))
  (when client
    (set! (client-tags client) tag)
    (focusclient (focustop (current-monitor)) #t)
    (arrange (current-monitor))))


(define* (list-scene-graph #:optional (tree (.tree (gwwm-scene))))
  (let loop ((obj tree))
    (cons obj
          (if (wlr-scene-tree? obj)
              (list (map (compose loop wlr-scene-object-from-node)
                         (wlr-scene-tree-children obj)))
              (list)))))
(export list-scene-graph)

(define (arrange m)
  (for-each
   (lambda (c)
     (let ((cm (client-monitor c)))
       (when cm
         (client-scene-set-enabled c (visibleon c cm))
         (cond ((client-fullscreen? c)
                (client-resize c (shallow-clone (monitor-area cm)) #f)
                (wlr-scene-node-reparent
                 (.node (client-scene c))
                 (.node fullscreen-layer)))
               ((client-floating? c)
                (wlr-scene-node-reparent (.node (client-scene c)) (.node float-layer)))
               (else
                (wlr-scene-node-reparent (.node (client-scene c)) (.node tile-layer)))))))
   (client-list))
  (and=> (list-ref (monitor-layouts m) (monitor-sellt m))
         (lambda (lay)
           (and=> (layout-procedure lay)
                  (cut <> m))))
  ((@@ (gwwm) motionnotify) 0))

(define* (togglefullscreen #:optional (client (current-client)))
  (when client
    (client-do-set-fullscreen
     client (not (client-fullscreen? client)))))

(define* (setlayout layout #:optional (m (current-monitor)))
  (unless (equal? (list-ref (monitor-layouts m) (monitor-sellt m)) layout)
    (set! (monitor-sellt m) (logxor (monitor-sellt m)))
    (list-set! (monitor-layouts m) (monitor-sellt m) layout)
    (arrange m)))


(define* (togglefloating #:optional (client (current-client)))
  (when (and client (not (client-fullscreen? client)))
    (client-do-set-floating
     client
     (not (client-floating? client)))))

(define* (incmaster num #:optional (m (current-monitor)))
  (set! (monitor-nmaster m )
        (max (+ (monitor-nmaster m) num) 0))
  (arrange m))

(define (focusclient client lift)
  ((@@ (gwwm) focusclient) client lift))

(define* (focusstack bool)
  (let ((c (current-client))
        (m (current-monitor))
        (c-l (client-list)))
    (unless (or (not c)
                (<= (length c-l) 1)
                (and (client-fullscreen? c)
                     (lockfullscreen? )))
      (and=> (let/ec return
               (for-each (lambda (o)
                           (if (equal? c o)
                               (return #f)
                               (when (visibleon o m)
                                 (return o))))
                         ((if bool identity reverse)
                          (cdr (member c (append c-l c-l))))))
             (cut focusclient <> #t)))))

(define (focustop monitor)
  (let/ec return
    (for-each (lambda (c)
                (when (visibleon c monitor)
                  (return c)))
              (car (%fstack)))
    #f))

(define (moveresize n)
  (when (eq? (cursor-mode) 'normal)
    (grabc (maybe-ref (client-at (gwwm-cursor)) (const #f)))
    (let ((c (grabc))
          (cursor (gwwm-cursor)))
      (when (and c
                 (not (client-fullscreen? c)))
        (client-do-set-floating c #t)
        (cursor-mode n)
        (case n
          ((move)
           (grabcx (inexact->exact
                    (round (- (.x cursor)
                              (box-x (client-geom c))))))
           (grabcy (inexact->exact
                    (round (- (.y cursor)
                              (box-y (client-geom c))))))
           (wlr-cursor-set-xcursor
            cursor (gwwm-xcursor-manager) "fleur")
           (arrange (current-monitor)))
          ;; ((0) 'do-nothing)
          ((resize)
           (client-set-resizing! c #t)
           (let-slots (client-geom c) (x y width height )
             (wlr-cursor-warp-closest cursor #f
                                      (+ x width)
                                      (+ y height)))
           (wlr-cursor-set-xcursor
            cursor
            (gwwm-xcursor-manager)
            "bottom_right_corner")))))))

(define* (killclient #:optional (client (current-client)))
  (and=> client client-send-close))
(define (chvt num)
  (wlr-session-change-vt
   (gwwm-session)
   num))

(define (gwwm-quit)
  (send-log INFO "try quit")
  (wl-display-terminate (gwwm-display)))
