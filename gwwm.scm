(define-module (gwwm)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-189)
  #:use-module (wlroots util box)
  #:use-module (util572 box )
  #:use-module (ice-9 format)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots types input-inhibitor)
  #:use-module (wlroots types data-device)
  #:use-module (wlroots types keyboard)
  #:use-module (wlroots types output-management)
  #:use-module (wlroots render allocator)
  #:use-module (system repl server)
  #:use-module ((system foreign ) #:select (make-pointer))
  #:use-module (gwwm keymap)
  #:use-module (gwwm keyboard)
  #:use-module (gwwm i18n)
  #:use-module (gwwm client)
  #:use-module (gwwm monitor)
  #:use-module (gwwm layout)
  #:use-module (gwwm layout tile)
  #:use-module (gwwm listener)
  #:use-module (gwwm utils)
  #:use-module (gwwm utils srfi-215)
  #:use-module (gwwm utils ref)
  #:use-module (wayland display)
  #:use-module (wayland protocol)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wlroots xwayland)
  #:use-module (wlroots backend)
  #:use-module (wlroots types)
  #:use-module (wlroots types pointer)
  #:use-module (wlroots types scene)
  #:use-module (wlroots types idle)
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types cursor)
  #:use-module (wlroots types xcursor)
  #:use-module (wlroots types output)
  #:use-module (wlroots types seat)
  #:use-module (wlroots types layer-shell)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types xdg-activation)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots types presentation-time)
  #:use-module (gwwm configuration)
  #:use-module (gwwm config)
  #:use-module (gwwm hooks)
  #:use-module (gwwm commands)
  #:use-module ((bytestructure-class) #:select (bs:enum->integer))
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (main))

(eval-when (expand load eval)
  (load-extension "libgwwm" "scm_init_gwwm"))


(define-public (keymap-global-set key command)
  (keymap-set (global-keymap) key command))

(define-syntax-rule (define-dy name objname)
  (begin (define-once name
           (let ((%o #f))
             (lambda* (#:optional objname)
               (if objname
                   (begin
                     (set! %o objname)
                     %o)
                   %o))))
         (export name)))

(define-dy gwwm-display display)
(define-dy gwwm-backend backend)
(define-dy gwwm-output-layout output-layout)
(define-dy entire-layout-box box)
(define-dy gwwm-renderer renderer)
(define-dy gwwm-allocator allocator)
(define-dy gwwm-cursor cursor)
(define-dy gwwm-seat seat)
(define-dy gwwm-scene scene)
(define-dy gwwm-xdg-shell xdg-shell)
(define-dy gwwm-xcursor-manager manager)
(define-dy gwwm-compositor compositor)
(define-dy gwwm-xwayland xwayland)
(define-dy gwwm-activation activation)
(define-dy gwwm-layer-shell layer-shell)
(define-dy gwwm-idle idle)
(define-dy gwwm-input-inhibit-manager input-inhibit-manager)
(define-once exclusive-focus
  (let ((%o (nothing)))
    (lambda* (#:optional (surface (nothing)))
      (if (nothing? surface)
          (maybe-ref/default %o #f)
          (begin
            (set! %o (just surface))
            surface)))))
(define-dy gwwm-output-manager mgr)

(define (init-global-keybind)
  (keymap-global-set (kbd (s S space))

                     togglefloating)
  (keymap-global-set (kbd (s S c))
                     killclient)

  (keymap-global-set
   (kbd (s f))
   togglefullscreen)
  (keymap-global-set
   (kbd (s j))
   (lambda ()
     (focusstack 1)))
  (keymap-global-set
   (kbd (s k))
   (lambda ()
     (focusstack -1)))
  (keymap-global-set
   (kbd (s e))
   (lambda ()
     (spawn "emacs")))
  (keymap-global-set
   (kbd (s Tab))
   zoom)
  (keymap-global-set
   (kbd (s S q))
   gwwm-quit)
  (for-each (lambda (a)
              (keymap-global-set
               (kbd* `(C M ,(string->symbol (string-append
                                             "F" (number->string a)))))
               (lambda () (chvt a))))
            (iota 12 1))
  (define (tagkeys k)
    (keymap-global-set (kbd* `(s ,k)) (lambda () (view k)))
    (keymap-global-set (kbd* `(C s ,k)) (lambda () (toggleview k)))
    (keymap-global-set (kbd* `(s S ,k)) (lambda () (tag k)))
    (keymap-global-set (kbd* `(C s S ,k)) (lambda () (toggletag k))))
  (for-each tagkeys (iota 10 0)))
(define option-spec
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))))
(define-public (parse-command-line)
  (let* ((options (getopt-long (command-line) option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f)))
    (if (or version-wanted help-wanted)
        (begin (when version-wanted
                 (display (string-append "gwwm " %version "\n")))
               (when help-wanted
                 (display (G_ "\
gwwm [options]
  -v --version  Display version
  -h --help     Display this help
")))
               (exit 0)))))
(define-once global-keymap
  (make-parameter (make-keymap)))
(define (setup-server)
  (false-if-exception (spawn-server (make-tcp-server-socket))))
;; (primitive-load-path "gwwm/startup.scm")

(define (setup-socket)
  (let ((socket (wl-display-add-socket-auto (gwwm-display))))
    (if socket
        (begin (setenv "WAYLAND_DISPLAY" socket)
               (send-log DEBUG
                         (format #f (G_ "set WAYLAND_DISPLAY to ~S.") socket)
                         'SOCKET socket))
        (begin
          (send-log EMERGENCY (G_ "wl-display-add-socket-auto fail.") 'SOCKET socket)
          (exit 1)))))

(define (config-setup)
  (add-to-load-path
   (string-append
    (get-xdg-config-home)
    "/" "gwwm"))
  (%config-setup))

(define (closemon m)
  (for-each (lambda (c)
              (let ((geom (client-geom c)))
                (when (and (client-is-floating? c)
                           (> (box-x geom)
                              (box-width (monitor-area m))))
                  (client-resize c (modify-instance geom
                                     (x (- x (box-width (monitor-window-area m))))) #t))
                (when (equal? (client-monitor c) m)
                  (setmon
                   c
                   (current-monitor)
                   (client-tags c)))))
            (client-list)))

(define-public background-layer #f)
(define-public bottom-layer #f)
(define-public tile-layer #f)
(define-public float-layer #f)
(define-public top-layer #f)
(define-public overlay-layer #f)
(define-public no-focus-layer #f)

(define (setmon c m newtag)
  (let ((old (client-monitor c))
        (surface (client-surface c)))
    (unless (equal? old m)
      (set! (client-monitor c) m)
      (when old
        (wlr-surface-send-leave surface
                                (monitor-wlr-output old))
        (arrange old))
      (when m
        (client-resize c (client-geom c) #f)
        (wlr-surface-send-enter surface (monitor-wlr-output m))
        (set! (client-tags c)
              (if (zero? newtag)
                  (list-ref (%monitor-tagset m)
                            (%monitor-seltags m))
                  newtag))
        (arrange m))
      (focusclient (focustop (current-monitor)) #t))))

(define (update-monitor m config)
  (let* ((output (monitor-wlr-output m))
         (config-head (wlr-output-configuration-head-v1-create config output))
         (box (wlr-output-layout-get-box (gwwm-output-layout) output))
         (state (.state config-head)))
    (set! (monitor-area m) box)
    (set! (monitor-window-area m) (shallow-clone box))
    (wlr-scene-output-set-position
     (monitor-scene-output m)
     (box-x box)
     (box-y box))
    (arrangelayers m)
    (arrange m)

    (modify-instance* state
      (x (box-x box))
      (y (box-y box))
      (enabled (.enabled output))
      (scale (.scale output)))))

(define (update-monitors listener data)
  (let ((config (wlr-output-configuration-v1-create)))
    (entire-layout-box (wlr-output-layout-get-box (gwwm-output-layout)))
    (for-each (cut update-monitor <> config) (monitor-list))
    (and-let* ((m (current-monitor))
               ((.enabled (monitor-wlr-output m))))
      (for-each (lambda (c)
                  (when (and (not (client-monitor c)) (client-mapped? c))
                    (setmon c m (client-tags c)))) (client-list)))
    (wlr-output-manager-v1-set-configuration (gwwm-output-manager) config)))

(define (create-keyboard device)
  (let ((kb (%create-keyboard device)))
    ((@@ (gwwm keyboard) add-keyboard) kb)
    (add-listen* (.device device) 'modifiers
                 (lambda (listener data)
                   (let ((wlr-keyboard (wrap-wlr-keyboard data)))
                     (wlr-seat-set-keyboard
                      (gwwm-seat) device)
                     (run-hook modifiers-event-hook kb)
                     (wlr-seat-keyboard-notify-modifiers
                      (gwwm-seat) (.modifiers wlr-keyboard))))
                 #:destroy-when device)
    (add-listen* (.device device) 'key
                 (lambda (listener data)
                   (let* ((event (wrap-wlr-event-keyboard-key data))
                          (seat (gwwm-seat))
                          (keybinding (@@ (gwwm keybind) keybinding)))
                     (run-hook keypress-event-hook kb event)

                     (unless (and (not (.active-inhibitor
                                        (gwwm-input-inhibit-manager)))
                                  (= (.state event) 1)
                                  (keybinding
                                   (wlr-keyboard-get-modifiers (.device device))
                                   (+ 8 (.keycode event))))
                       (wlr-seat-set-keyboard seat device)
                       (wlr-seat-keyboard-notify-key
                        seat
                        (.time-msec event)
                        (.keycode event)
                        (.state event)))))
                 #:destroy-when device)
    (add-listen* device 'destroy
                 (lambda (listener data)
                   (run-hook cleanup-keyboard-hook kb)
                   ((@@ (gwwm keyboard) remove-keyboard) kb)))))

(define (request-start-drag listener data)
  (let* ((event (wrap-wlr-seat-request-start-drag-event data))
         (seat (~ event 'drag 'seat)))
    (if (wlr-seat-validate-pointer-grab-serial
         seat
         (.origin event)
         (.serial event))
        (wlr-seat-start-pointer-drag seat (.drag event) (.serial event))
        (wlr-data-source-destroy (~ event 'drag 'source)))))

(define* (motionnotify #:optional (time 0))
  (when (zero? time)
    (idle-activity))
  (when (gwwm-sloppyfocus?)
    (set! (current-monitor) (monitor-at (.x (gwwm-cursor)) (.y (gwwm-cursor)))))
  (run-hook motion-notify-hook time)
  (%motionnotify time))

(define (idle-activity . _) (wlr-idle-notify-activity (gwwm-idle) (gwwm-seat)))
(define (gwwm-setup)
  (gwwm-display (wl-display-create))
  (or (and=> (wlr-backend-autocreate(gwwm-display)) gwwm-backend)
      (begin (send-log ERROR (G_ "gwwm Couldn't create backend"))
             (exit 1)))
  (or (and=> (wlr-renderer-autocreate (gwwm-backend)) gwwm-renderer)
      (begin (send-log ERROR (G_ "gwwm Couldn't create renderer"))
             (exit 1)))
  (wlr-renderer-init-wl-display (gwwm-renderer) (gwwm-display))
  (or (and=> (wlr-allocator-autocreate
              (gwwm-backend)
              (gwwm-renderer)) gwwm-allocator)
      (begin (send-log ERROR (G_ "gwwm Couldn't create allocator"))
             (exit 1)))
  (gwwm-cursor (wlr-cursor-create))

  (add-listen* (gwwm-cursor) 'axis
               (lambda (listener data)
                 (let ((event (wrap-wlr-event-pointer-axis data)))
                   (run-hook axis-event-hook event)
                   (let-slots event (time-msec orientation delta delta-discrete
                                               source)
                     (wlr-seat-pointer-notify-axis
                      (gwwm-seat)
                      time-msec
                      orientation
                      delta
                      delta-discrete
                      source))
                   (idle-activity)))
               #:remove-when-destroy? #f)
  (add-listen* (gwwm-cursor) 'frame
               (lambda (listener data)
                 "This event is forwarded by the cursor when a pointer emits
an frame event. Frame events are sent after regular pointer events to group
multiple events together. For instance, two axis events may happen at the same
time, in which case a frame event won't be sent in between. Notify the client
with pointer focus of the frame event."
                 (let ((cursor (wrap-wlr-cursor data)))
                   (run-hook cursor-frame-event-hook cursor)
                   (wlr-seat-pointer-notify-frame (gwwm-seat))))
               #:remove-when-destroy? #f)
  (add-listen* (gwwm-cursor) 'motion (lambda (listener data)
                                       (let ((event (wrap-wlr-event-pointer-motion data)))
                                         (wlr-cursor-move (gwwm-cursor)
                                                          (.device event)
                                                          (.delta-x event)
                                                          (.delta-y event))
                                         (motionnotify (.time-msec event))))
               #:remove-when-destroy? #f)
  (add-listen* (gwwm-cursor) 'motion-absolute
               (lambda (listener data)
                 (let ((event (wrap-wlr-event-pointer-motion-absolute data)))
                   (let-slots event (device x y time-msec)
                     (wlr-cursor-warp-absolute (gwwm-cursor) device x y)
                     (motionnotify time-msec))))
               #:remove-when-destroy? #f)
  (add-listen* (gwwm-cursor) 'button
               (lambda (listener data)
                 (let ((event (wrap-wlr-event-pointer-button data)))
                   (idle-activity)
                   (run-hook cursor-button-event-hook event)
                   (buttonpress listener data)))
               #:remove-when-destroy? #f)
  (define (add-seat-capabilitie seat o)
    (wlr-seat-set-capabilities
     seat
     (logior (.capabilities seat)
             (bs:enum->integer %wl-seat-capability-enum o))))
  (add-listen* (gwwm-backend) 'new-input
               (lambda (listener data)
                 (let ((device (wrap-wlr-input-device data)))
                   (case (.type device)
                     ((WLR_INPUT_DEVICE_KEYBOARD)
                      (create-keyboard device)
                      (unless (zero? (length (keyboard-list)))

                        (add-seat-capabilitie
                         (gwwm-seat)
                         'WL_SEAT_CAPABILITY_KEYBOARD)))
                     ((WLR_INPUT_DEVICE_POINTER)
                      (create-pointer device)
                      (add-seat-capabilitie
                       (gwwm-seat)
                       'WL_SEAT_CAPABILITY_POINTER))
                     ((WLR_INPUT_DEVICE_TOUCH)
                      (send-log WARNING "TODO"))
                     ((WLR_INPUT_DEVICE_SWITCH)
                      (send-log WARNING "TODO"))
                     ((WLR_INPUT_DEVICE_TABLET_TOOL)
                      (send-log WARNING "TODO"))
                     ((WLR_INPUT_DEVICE_TABLET_PAD)
                      (send-log WARNING "TODO"))
                     (else (send-log WARNING "unknow input device"))))))
  (add-listen* (gwwm-backend) 'new-output create-monitor)
  (gwwm-xcursor-manager (wlr-xcursor-manager-create #f 24))
  (gwwm-seat (wlr-seat-create (gwwm-display) "seat0"))
  (add-listen* (gwwm-seat) 'request-set-cursor setcursor)
  (add-listen* (gwwm-seat) 'request-set-selection
               (lambda (listener data)
                 (let ((event (wrap-wlr-seat-request-set-selection-event data)))
                   (run-hook selection-hook event)
                   (wlr-seat-set-selection (gwwm-seat) (.source event) (.serial event)))))
  (add-listen* (gwwm-seat) 'request-start-drag request-start-drag)
  (add-listen* (gwwm-seat) 'request-set-primary-selection setpsel)
  (add-listen* (gwwm-seat)
               'start-drag
               ;; startdrag
               (lambda (listener data)
                 (and-let* ((drag (wrap-wlr-drag data))
                            (icon (.icon drag))
                            (scene (wlr-scene-subsurface-tree-create
                                    no-focus-layer
                                    (.surface icon)))
                            (drag-move
                             (lambda _
                               (wlr-scene-node-set-position
                                scene
                                (inexact->exact
                                 (round (+ (.x (gwwm-cursor))
                                           (.sx (.surface icon)))))
                                (inexact->exact
                                 (round (+ (.y (gwwm-cursor))
                                           (.sy (.surface icon)))))))))

                   (add-hook! motion-notify-hook drag-move)
                   (motionnotify)
                   (add-listen* icon 'destroy
                                (lambda (listener data)
                                  (remove-hook! motion-notify-hook drag-move)
                                  (wlr-scene-node-destroy scene)
                                  (focusclient (current-client) #t)
                                  (motionnotify))
                                #:remove-when-destroy? #f))))
  (gwwm-xdg-shell (wlr-xdg-shell-create (gwwm-display)))
  (add-listen* (gwwm-xdg-shell) 'new-surface create-notify)
  (gwwm-compositor (wlr-compositor-create (gwwm-display) (gwwm-renderer)))
  (gwwm-activation (wlr-xdg-activation-v1-create (gwwm-display)))
  (add-listen* (gwwm-activation) 'request-activate
               (lambda (listener data)
                 (let* ((event (wrap-wlr-xdg-activation-v1-request-activate-event data))
                        (c (client-from-wlr-surface (.surface event))))
                   (pk 's)
                   (unless (equal? c (current-client))
                     (set! (client-urgent? c) #t)))))
  (gwwm-layer-shell (wlr-layer-shell-v1-create (gwwm-display)))
  (add-listen* (gwwm-layer-shell) 'new-surface create-layer-client)
  (gwwm-idle (wlr-idle-create (gwwm-display)))
  (gwwm-output-layout (wlr-output-layout-create))
  (add-listen* (gwwm-output-layout) 'change update-monitors)
  (gwwm-output-manager (wlr-output-manager-v1-create (gwwm-display)))

  (add-hook! keypress-event-hook idle-activity)
  (add-listen* (gwwm-output-manager) 'apply (lambda (listener data)
                                              (let ((config (wrap-wlr-output-configuration-v1 data)))
                                                (output-manager-apply-or-test config #f))))
  (add-listen* (gwwm-output-manager) 'test (lambda (listener data)
                                             (let ((config (wrap-wlr-output-configuration-v1 data)))
                                               (output-manager-apply-or-test config #t))))
  (wlr-cursor-attach-output-layout (gwwm-cursor) (gwwm-output-layout)))
(define (xwayland-setup)
  (let ((x (gwwm-xwayland (wlr-xwayland-create (gwwm-display) (gwwm-compositor) #t))))
    (if x
        (begin
          (wl-signal-add (get-event-signal x 'ready)
                         xwaylandready)
          (wl-signal-add (get-event-signal x 'new-surface)
                         new-xwayland-surface)
          (setenv "DISPLAY" (wlr-xwayland-display-name x)))
        (send-log INFO (G_ "failed to setup XWayland X server, continuing without it.")))))
(define (map-notify* c)
  (lambda (listener data)
    (run-hook client-map-event-hook c
              ((if (client-is-x11? c)
                   wrap-wlr-xwayland-surface
                   wrap-wlr-xdg-surface)
               data))
    (when (client-is-x11? c)
      (add-listen* (.surface (client-super-surface c)) 'destroy
                   (lambda (listener data)
                     (destroy-surface-notify c listener data))
                   #:remove-when-destroy? #f)
      (set! (client-surface c)
            (wlr-xwayland-surface-surface
             (client-super-surface c))))
    (set! (client-scene c)
          (.node (wlr-scene-tree-create tile-layer)))
    (set! (client-scene-surface c)
          (if (is-a? c <gwwm-xdg-client>)
              (wlr-scene-xdg-surface-create
               (client-scene c)
               (client-super-surface c))
              (wlr-scene-subsurface-tree-create
               (client-scene c)
               (client-surface c))))
    (let ((geom (client-get-geometry c)))
      (set! (client-geom c) geom)
      (set! (client-prev-geom c) (shallow-clone geom)))
    (add-listen* (client-surface c) 'commit
                 (lambda (a b)
                   (let ((client c))
                     (run-hook surface-commit-event-hook client)
                     (unless (client-is-x11? client)
                       (let ((serial (client-resize-configure-serial client))
                             (current (.current
                                       (client-super-surface c)))
                             (pending (.pending
                                       (client-super-surface c))))
                         (when (and (not (zero? serial))
                                    (or (<= serial (.configure-serial current))
                                        (and (= (box-width (.geometry current))
                                                (box-width (.geometry pending)))
                                             (= (box-height (.geometry current))
                                                (box-height (.geometry pending))))))
                           (set! (client-resize-configure-serial client) 0)))))))
    (set! (.data (client-surface c))
          (get-pointer (client-scene c)))
    (let ((p (make-pointer (~ c 'data))))
      (set! (.data (client-scene c)) p)
      (set! (.data (client-scene-surface c)) p))
    (if (client-is-unmanaged? c)
        (begin (wlr-scene-node-reparent (client-scene c) float-layer)
               (wlr-scene-node-set-position
                (client-scene c)
                (+ (.x (client-geom c)) (gwwm-borderpx))
                (+ (.y (client-geom c)) (gwwm-borderpx))))
        (begin (client-init-border c)
               (q-push! (%clients) c)
               (q-push! (%fstack) c)

               (let ((parent (client-get-parent c)))
                 (if parent
                     (begin (setmon c (or (client-monitor parent)
                                          (current-monitor))
                                    (client-tags parent))
                            (client-do-set-floating c #t))
                     (%applyrules c)))
               (client-do-set-fullscreen c )
               (map-notify c listener data)))))

(define (map-layer-client-notify c)
  (lambda (listener data)
    (wlr-surface-send-enter
     (client-surface c)
     (monitor-wlr-output(client-monitor c)))
    (motionnotify)))
(define (unmap-layer-client-notify c)
  (lambda (listener data)
    (wlr-scene-node-set-enabled (client-scene c) #f)
    (when (equal? (client-surface c) (exclusive-focus))
      (exclusive-focus #f))
    (when (equal? (client-surface c)
                  (.focused-surface
                   (.keyboard-state (gwwm-seat))))
      (focusclient (current-client) #f))
    (motionnotify)))
(define (main)
  (setlocale LC_ALL "")
  (textdomain %gettext-domain)

  (add-hook! create-monitor-hook
             (lambda (m)
               (let ((output (monitor-wlr-output m)))
                 (wlr-output-set-mode output (wlr-output-preferred-mode output))
                 (wlr-output-enable output #t)
                 (wlr-output-enable-adaptive-sync (monitor-wlr-output m) #t))

               (set! (monitor-layouts m)
                     (make-list 2 tile-layout))
               (add-listen* (monitor-wlr-output m) 'frame
                            (lambda (listener data)
                              (render-monitor-notify m listener data)))
               (add-listen* (monitor-wlr-output m) 'destroy
                            (lambda (listener data)
                              (q-remove! (%monitors) m)
                              (wlr-output-layout-remove
                               (gwwm-output-layout)
                               (monitor-wlr-output m))
                              (cleanup-monitor m listener data)
                              (focusclient (focustop (current-monitor)) #t)
                              (closemon m)))))

  (define (commit-event c)
    (let ((box (client-get-geometry c))
          (m (client-monitor c)))
      (if (and m (not (box-empty? box))
               (or (not (= (box-width box)
                           (- (box-width (client-geom c))
                              (* 2 (client-border-width c)))))
                   (not (= (box-height box)
                           (- (box-height (client-geom c))
                              (* 2 (client-border-width c)))))))
          (arrange m))))
  (add-hook! surface-commit-event-hook
             commit-event)
  (current-log-callback
   (let ((p (current-error-port)))
     (lambda (msg)
       (let ((msg2 msg))
         (format p "~a: [~a]| ~a | "
                 (date->string (current-date) "~m-~d ~3 ~N")
                 (cdr (assq 'SEVERITY msg))
                 (cdr (assq 'MESSAGE msg)))
         (set! msg2 (assoc-remove! (assoc-remove! msg2 'SEVERITY) 'MESSAGE))
         (for-each (lambda (a)
                     (display (car a) p)
                     (display ":" p)
                     (display (object->string(cdr a)) p)
                     (display " " p))
                   msg2)
         (newline p)))))
  (add-hook! create-client-hook
             (lambda (c)
               (send-log DEBUG "client createed" 'CLIENT c)))
  (define new-popup-notify*
    (lambda (c)
      (lambda (listener data)
        (let* ((popup (wrap-wlr-xdg-popup data)))
          (add-listen* (.base popup) 'new-popup (new-popup-notify* c))
          (run-hook create-popup-hook popup)
          (new-popup-notify listener data)
          (and-let* ((monitor (client-monitor c))
                     (geom (shallow-clone
                            (if (is-a? c <gwwm-layer-client>)
                                (monitor-area monitor)
                                (monitor-window-area (client-monitor c))))))
            (modify-instance* geom
              (x (- x (box-x (client-geom c))))
              (y (- y (box-y (client-geom c)))))
            (wlr-xdg-popup-unconstrain-from-box popup geom))))))

  (define (set-title-notify c)
    (lambda (listener data)
      (let ((title (client-title c))
            (new (client-get-title c)))
        (set! (client-title c) new )
        (run-hook update-title-hook c title new))))

  (define (request-fullscreen-notify c)
    (lambda (listener data)
      (let ((fullscreen? (client-wants-fullscreen? c))
            (event (wrap-wlr-xdg-toplevel-set-fullscreen-event data)))
        (if (client-monitor c)
            (client-do-set-fullscreen c fullscreen?)
            (set! (client-fullscreen? c) fullscreen?))
        (run-hook fullscreen-event-hook c event))))
  (add-hook!
   create-client-hook
   (lambda (c)
     (when (is-a? c <gwwm-client>)
       (set! (client-appid c) (client-get-appid c))
       (set! (client-title c) (client-get-title c))
       (add-listen* (client-super-surface c) 'unmap
                    (lambda (listener data)
                      (unmap-notify c listener data)
                      (unless (client-is-unmanaged? c)
                        (setmon c #f 0)
                        (q-remove! (%clients) c)
                        (q-remove! (%fstack) c)
                        (wlr-scene-node-destroy (client-scene c)))))
       (add-listen* (client-super-surface c) 'map (map-notify* c)))
     (cond ((is-a? c <gwwm-xdg-client>)
            (add-listen* (client-surface c) 'destroy
                         (lambda (listener data)
                           (destroy-surface-notify c listener data))
                         #:remove-when-destroy? #f)
            (add-listen* (client-super-surface c) 'new-popup
                         (new-popup-notify* c))
            (add-listen* (wlr-xdg-surface-toplevel (client-super-surface c))
                         'set-title (set-title-notify c)
                         #:destroy-when (client-super-surface c))

            (add-listen* (wlr-xdg-surface-toplevel (client-super-surface c))
                         'set-app-id
                         (lambda (listener data)
                           (set! (client-appid c)
                                 (client-get-appid c)))
                         #:destroy-when (client-super-surface c))
            (add-listen* (wlr-xdg-surface-toplevel (client-super-surface c))
                         'request-fullscreen
                         (request-fullscreen-notify c)
                         #:destroy-when (client-surface c))

            )
           ((is-a? c <gwwm-x-client>)
            (add-listen* (client-super-surface c) 'set-app-id
                         (lambda (listener data)
                           (set! (client-appid c)
                                 (client-get-appid c)))
                         #:destroy-when (client-super-surface c)))
           ((is-a? c <gwwm-layer-client>)
            (q-push! (%layer-clients) c)
            (add-listen* (client-super-surface c) 'map
                         (map-layer-client-notify c))
            (add-listen* (client-surface c) 'destroy
                         (lambda (listener data)
                           (destroy-surface-notify c listener data))
                         #:remove-when-destroy? #f)
            (add-listen* (.surface (client-super-surface c)) 'commit
                         (lambda (listener data)
                           (commit-layer-client-notify c listener data)))
            (add-listen* (client-super-surface c) 'destroy
                         (lambda (listener data)
                           (q-remove! (%layer-clients) c)
                           (destroy-layer-client-notify c listener data)
                           (wlr-scene-node-destroy (client-scene c))
                           (and=> (client-monitor c) arrangelayers))
                         #:remove-when-destroy? #f)
            (add-listen* (client-super-surface c) 'unmap
                         (unmap-layer-client-notify c))))))
  (parse-command-line)
  (send-log DEBUG (G_ "init global keybind ..."))
  (init-global-keybind)
  (unless (getenv "XDG_RUNTIME_DIR")
    (send-log EMERGENCY (G_ "XDG_RUNTIME_DIR must be set."))
    (exit 1))
  (setvbuf (current-output-port) 'line)
  (setvbuf (current-error-port) 'line)
  (gwwm-setup)
  (gwwm-scene (wlr-scene-create))
  (wlr-scene-set-presentation
   (gwwm-scene)
   (wlr-presentation-create
    (gwwm-display)
    (gwwm-backend)))
  (let* ((scene (gwwm-scene))
         (create (lambda () (.node (wlr-scene-tree-create (.node scene))))))
    (set! background-layer (create))
    (set! bottom-layer (create))
    (set! tile-layer (create))
    (set! float-layer (create))
    (set! top-layer (create))
    (set! overlay-layer (create))
    (set! no-focus-layer (create))
    )
  (%gwwm-setup-signal)
  (%gwwm-setup-othres)
  (gwwm-input-inhibit-manager (wlr-input-inhibit-manager-create (gwwm-display)))
  (%gwwm-setup)
  (config-setup)
  (when (config-enable-xwayland? (gwwm-config))
    (xwayland-setup))
  (set-current-module (resolve-module '(guile-user)))
  (setup-server)
  (setup-socket)
  ;; Start the backend. This will enumerate outputs and inputs, become the DRM
  ;; master, etc
  (if (wlr-backend-start (gwwm-backend))
      (send-log INFO (G_ "backend is started."))
      (begin (send-log ERROR (G_ "gwwm cannot start backend!"))
             (exit 1)))
  (set! (current-monitor)
        (monitor-at
         (.x (gwwm-cursor))
         (.y (gwwm-cursor))))
  (wlr-cursor-warp-closest
   (gwwm-cursor) #f
   (.x (gwwm-cursor))
   (.y (gwwm-cursor)))
  (wlr-xcursor-manager-set-cursor-image
   (gwwm-xcursor-manager)
   (config-cursor-normal-image (gwwm-config) )
   (gwwm-cursor))
  (run-hook gwwm-after-init-hook)
  (wl-display-run (gwwm-display))
  (%gwwm-cleanup))
