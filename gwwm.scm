(define-module (gwwm)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-19)
  #:use-module (wlroots util box)
  #:use-module (util572 box )
  #:use-module (ice-9 format)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots render allocator)
  #:use-module (system repl server)
  #:use-module (gwwm keymap)
  #:use-module (gwwm i18n)
  #:use-module (gwwm client)
  #:use-module (gwwm monitor)
  #:use-module (gwwm layout)
  #:use-module (gwwm layout tile)
  #:use-module (gwwm listener)
  #:use-module (gwwm utils)
  #:use-module (gwwm utils srfi-215)
  #:use-module (wayland display)
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
  #:use-module (gwwm configuration)
  #:use-module (gwwm config)
  #:use-module (gwwm hooks)
  #:use-module (gwwm commands)
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
(define-dy exclusive-focus surface)

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
                (pk 1)
                (when (and (client-is-floating? c)
                           (> (box-x geom)
                              (box-width (monitor-area m))))
                  (client-resize c (modify-instance geom
                                     (x (- x (box-width (monitor-window-area m)))))
                                 #t))
                (pk 'bb)
                (when (equal? (client-monitor c) m)
                  (
                   %setmon
                   c
                   (current-monitor)
                   (client-tags c)))
                (pk '3)))
            (client-list)))

(define-public background-layer #f)
(define-public bottom-layer #f)
(define-public tile-layer #f)
(define-public float-layer #f)
(define-public top-layer #f)
(define-public overlay-layer #f)
(define-public no-focus-layer #f)

(define* (add-listen* obj symbol proc
                      #:key
                      (destroy-when obj)
                      (remove-when-destroy? #t))
  (let ((listener (make-wl-listener proc)))
    (wl-signal-add (get-event-signal obj symbol) listener)
    (when remove-when-destroy?
      (wl-signal-add
       (get-event-signal destroy-when 'destroy)
       (make-wl-listener
        (lambda _
          (wl-list-remove (.link listener))))))))

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
  (define (idle-activity) (wlr-idle-notify-activity (gwwm-idle) (gwwm-seat)))
  (add-listen* (gwwm-cursor) 'axis
               (lambda (listener data)
                 (let ((event (wrap-wlr-event-pointer-axis data)))
                   (run-hook axis-event-hook event)
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
  (add-listen* (gwwm-cursor) 'motion motionrelative
               #:remove-when-destroy? #f)
  (add-listen* (gwwm-cursor) 'motion-absolute
               motionabsolute
               #:remove-when-destroy? #f)
  (add-listen* (gwwm-cursor) 'button
               (lambda (listener data)
                 (let ((event (wrap-wlr-event-pointer-button data)))
                   (idle-activity)
                   (run-hook cursor-button-event-hook event)
                   (buttonpress listener data)))
               #:remove-when-destroy? #f)
  (add-listen* (gwwm-backend) 'new-input
               (lambda (listener data)
                 (let ((device (wrap-wlr-input-device data)))
                   (case (value->wlr-input-device-type
                          (wlr-input-device-type device))
                     ((WLR_INPUT_DEVICE_KEYBOARD)
                      (create-keyboard device))
                     ((WLR_INPUT_DEVICE_POINTER)
                      (create-pointer device))
                     ((WLR_INPUT_DEVICE_TOUCH)
                      (send-log WARNING "TODO"))
                     ((WLR_INPUT_DEVICE_SWITCH)
                      (send-log WARNING "TODO"))
                     ((WLR_INPUT_DEVICE_TABLET_TOOL)
                      (send-log WARNING "TODO"))
                     ((WLR_INPUT_DEVICE_TABLET_PAD)
                      (send-log WARNING "TODO"))
                     (else (send-log WARNING "unknow input device")))
                   (inputdevice listener data))))
  (add-listen* (gwwm-backend) 'new-output create-monitor)
  (gwwm-xcursor-manager (wlr-xcursor-manager-create #f 24))
  (gwwm-seat (wlr-seat-create (gwwm-display) "seat0"))
  (gwwm-xdg-shell (wlr-xdg-shell-create (gwwm-display)))
  (add-listen* (gwwm-xdg-shell) 'new-surface create-notify)
  (gwwm-compositor (wlr-compositor-create (gwwm-display) (gwwm-renderer)))
  (gwwm-activation (wlr-xdg-activation-v1-create (gwwm-display)))
  (gwwm-layer-shell (wlr-layer-shell-v1-create (gwwm-display)))
  (add-listen* (gwwm-layer-shell) 'new-surface create-layer-client)
  (gwwm-idle (wlr-idle-create (gwwm-display)))
  (gwwm-output-layout (wlr-output-layout-create))
  (add-listen* (gwwm-output-layout) 'change updatemons)
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
    (add-listen* (client-surface c) 'commit
                 (lambda (a b)
                   (let ((client c))
                     (run-hook surface-commit-event-hook client)
                     (unless (client-is-x11? client)
                       (client-mark-resize-done-p client)))))
    (map-notify c listener data)))

(define (map-layer-client-notify c)
  (lambda (listener data)
    (wlr-surface-send-enter
     (client-surface c)
     (monitor-wlr-output(client-monitor c)))
    (%motionnotify 0)))
(define (main)
  (setlocale LC_ALL "")
  (textdomain %gettext-domain)
  (define (set-mode m)
    (let ((output (monitor-wlr-output m)))
      (wlr-output-set-mode output (wlr-output-preferred-mode output))))
  (define (set-default-layout m)
    (set! (monitor-layouts m)
          (make-list 2 tile-layout)))
  (add-hook! create-monitor-hook
             (lambda (m)
               (wlr-output-enable-adaptive-sync (monitor-wlr-output m) #t)))
  (add-hook! create-monitor-hook set-mode)
  (add-hook! create-monitor-hook set-default-layout)
  (add-hook! create-monitor-hook
             (lambda (m)
               (add-listen* (monitor-wlr-output m) 'frame render-monitor-notify)
               (add-listen* (monitor-wlr-output m) 'destroy cleanup-monitor)))

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
  (define (pass-modifiers k)
    (wlr-seat-set-keyboard (gwwm-seat) (keyboard-input-device k)))
  (add-hook! axis-event-hook
             (lambda (event)
               (wlr-seat-pointer-notify-axis
                (gwwm-seat)
                (wlr-event-pointer-axis-time-msec event)
                (wlr-event-pointer-axis-orientation event)
                (wlr-event-pointer-axis-delta event)
                (wlr-event-pointer-axis-delta-discrete event)
                (wlr-event-pointer-axis-source event))))
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
  (add-hook! modifiers-event-hook pass-modifiers )
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
      (run-hook update-title-hook c)))
  (define (request-fullscreen-notify c)
    (lambda (listener data)
      (pk 'oooo)
      (let ((fullscreen? (client-wants-fullscreen? c))
            (event (wrap-wlr-xdg-toplevel-set-fullscreen-event data)))
        (if (client-monitor c)
            (client-do-set-fullscreen c fullscreen?)
            (set! (client-fullscreen? c) fullscreen?))
        (run-hook fullscreen-event-hook c event))))
  (add-hook!
   create-client-hook
   (lambda (c)
     (cond ((is-a? c <gwwm-xdg-client>)
            (add-listen* (client-super-surface c) 'new-popup
                         (new-popup-notify* c))
            (add-listen* (client-super-surface c) 'map (map-notify* c))
            (add-listen* (wlr-xdg-surface-toplevel (client-super-surface c))
                         'set-title
                         (set-title-notify c)
                         #:destroy-when (client-super-surface c))

            (add-listen* (wlr-xdg-surface-toplevel (client-super-surface c))
                         'request-fullscreen
                         (request-fullscreen-notify c)
                         #:destroy-when (client-surface c))

            (add-listen* (client-super-surface c) 'unmap
                         (lambda (listener data)
                           (unmap-notify c listener data))))
           ((is-a? c <gwwm-x-client>)
            (add-listen* (client-super-surface c) 'map (map-notify* c))
            (add-listen* (client-super-surface c) 'unmap
                         (lambda (listener data)
                           (unmap-notify c listener data))))
           ((is-a? c <gwwm-layer-client>)
            (add-listen* (client-super-surface c) 'map
                         (map-layer-client-notify c))
            (add-listen* (.surface (client-super-surface c)) 'commit
                         (lambda (listener data)
                           (commit-layer-client-notify c listener data)))
            (add-listen* (client-super-surface c) 'destroy
                         (lambda (listener data)
                           (destroy-layer-client-notify c listener data))
                         #:remove-when-destroy? #f)
            (add-listen* (client-super-surface c) 'unmap
                         (lambda (listener data)
                           (unmap-layer-client-notify c listener data)))))))
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
  (%gwwm-setup-scene)
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
         (wlr-cursor-x (gwwm-cursor))
         (wlr-cursor-y (gwwm-cursor))))
  (wlr-cursor-warp-closest
   (gwwm-cursor) #f
   (wlr-cursor-x (gwwm-cursor))
   (wlr-cursor-y (gwwm-cursor)))
  (wlr-xcursor-manager-set-cursor-image
   (gwwm-xcursor-manager)
   (config-cursor-normal-image (gwwm-config) )
   (gwwm-cursor))
  (wl-display-run (gwwm-display))
  (%gwwm-cleanup))
