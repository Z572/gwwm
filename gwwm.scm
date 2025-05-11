(define-module (gwwm)
  #:use-module ((system foreign) #:select (make-pointer scm->pointer))
  #:use-module (ice-9 control)
  #:use-module ((rnrs base) #:select (assert))
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 q)
  #:use-module (oop goops describe)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-145)
  #:use-module (srfi srfi-189)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (system repl server)
  #:use-module (util572 box)
  #:use-module (libinput)
  #:use-module (bytestructures guile)
  #:use-module (bytestructure-class)
  #:use-module (xkbcommon xkbcommon)
  #:use-module (wayland server display)
  #:use-module (wayland list)
  #:use-module (wayland server listener)
  #:use-module (wayland server protocol wayland)
  #:use-module (wayland signal)
  #:use-module ((wlroots types scene) #:hide (.state))
  #:use-module (wlroots backend libinput)
  #:use-module (wlroots backend wayland)
  #:use-module (wlroots backend multi)
  #:use-module (wlroots backend)
  #:use-module (wlroots types gamma-control)
  #:use-module (wlroots render allocator)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots time)
  #:use-module (wlroots types idle-notify)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types export-dmabuf)
  #:use-module (wlroots types cursor)
  #:use-module (wlroots types data-control)
  #:use-module (wlroots types data-device)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots types input-inhibitor)
  #:use-module (wlroots types keyboard)
  #:use-module (wlroots types layer-shell)
  #:use-module (wlroots types touch)
  #:use-module (wlroots types output)
  #:use-module (wlroots types server-decoration)
  #:use-module (wlroots types screencopy)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots types output-management)
  #:use-module (wlroots types pointer)
  #:use-module (wlroots types pointer-gestures)
  #:use-module (wlroots types presentation-time)
  #:use-module (wlroots types primary-selection)
  #:use-module (wlroots types seat)
  #:use-module (wlroots types subcompositor)
  #:use-module (wlroots types viewporter)
  #:use-module (wlroots types xcursor-manager)
  #:use-module (wlroots types xdg-activation)
  #:use-module (wlroots types xdg-output)
  #:use-module (wlroots types xdg-decoration)
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types buffer)
  #:use-module (wlroots types)
  #:use-module (wlroots util box)
  #:use-module (gwwm client)
  ;; #:use-module (gwwm x-client)
  #:use-module (gwwm commands)
  #:use-module (gwwm config)
  #:use-module (gwwm configuration)
  #:use-module (gwwm hooks)
  #:use-module (gwwm i18n)
  #:use-module (gwwm keyboard)
  #:use-module (gwwm touch)
  #:use-module (gwwm pointer)
  #:use-module (gwwm keymap)
  #:use-module (gwwm layout tile)
  #:use-module (gwwm layout)
  #:use-module (gwwm listener)
  #:use-module (gwwm monitor)
  #:use-module (gwwm utils ref)
  #:use-module (gwwm utils srfi-215)
  #:use-module (gwwm utils)
  #:use-module (ice-9 eval-string)
  #:use-module (ice-9 suspendable-ports)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)

  #:use-module  (gwwm buffer)
  #:use-module  (cairo)

  #:export (main keymap-global-set

                 background-layer
                 bottom-layer
                 tile-layer
                 fullscreen-layer
                 float-layer
                 top-layer
                 overlay-layer
                 no-focus-layer))

(eval-when (expand load eval)
  (load-extension "libgwwm" "scm_init_gwwm"))


(define* (keymap-global-set key command #:optional release-command)
  (if release-command
      (keymap-set (global-keymap) key command release-command)
      (keymap-set (global-keymap) key command)))

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

(define-dy gwwm-config config)
(define-dy gwwm-display display)
(define-dy gwwm-backend backend)
(define-dy gwwm-session session)
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
(define-dy gwwm-activation activation)
(define-dy gwwm-layer-shell layer-shell)
(define-dy gwwm-idle idle)
(define-dy gwwm-data-control-manager m)
(define-dy gwwm-pointer-gestures g)
(define-dy gwwm-input-inhibit-manager input-inhibit-manager)
(define-dy gwwm-scene-output-layout l)
(define-dy grabc c)
(define-dy grabcx x)
(define-dy grabcy y)

(define-dy cursor-mode m)
(cursor-mode 'normal)

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
  (keymap-global-set (kbd (s S space)) togglefloating)
  (keymap-global-set (kbd (s S c)) killclient)

  (keymap-global-set (kbd (s f)) togglefullscreen)
  (keymap-global-set (kbd (s j)) (lambda () (focusstack 1)))
  (keymap-global-set (kbd (s k)) (lambda () (focusstack -1)))
  (keymap-global-set (kbd (s bracketleft)) (lambda () (incmaster -1)))
  (keymap-global-set (kbd (s bracketright)) (lambda () (incmaster 1)))
  (keymap-global-set (kbd (s e)) (lambda () (spawn "emacs" '("emacs"))))
  (keymap-global-set (kbd (s mouse-left)) (lambda () (moveresize 'move)))
  (keymap-global-set (kbd (s mouse-middle)) togglefloating)
  (keymap-global-set (kbd (s mouse-right)) (lambda () (moveresize 'resize)))
  (keymap-global-set (kbd (s S q)) gwwm-quit)
  (for-each (lambda (a)
              (keymap-global-set
               (kbd* `(C M ,(string->symbol (string-append
                                             "F" (number->string a)))))
               (lambda () (chvt a))))
            (iota 12 1))
  (define (tagkeys k)
    (keymap-global-set (kbd* `(s ,k)) (lambda () (view k)))
    (keymap-global-set (kbd* `(s S ,k)) (lambda () (tag k)))
    (keymap-global-set (kbd* `(C s S ,k)) (lambda () (toggletag k))))
  (for-each tagkeys (iota 10 0)))
(define option-spec
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (eval (single-char #\e) (value #t))
    (load (single-char #\l) (value #t))))
(define-public (parse-command-line args)
  (let* ((options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f))
         (eval-wanted (option-ref options 'eval #f))
         (load-wanted (option-ref options 'load #f)))
    (when load-wanted
      (let ((o (delay (primitive-load load-wanted))))
        (add-hook! gwwm-after-init-hook (lambda _ (force o)))))
    (when eval-wanted
      (let ((o (delay (eval-string eval-wanted #:module (resolve-module '(gwwm))))))
        (add-hook! gwwm-after-init-hook (lambda _ (force o)))))
    (if (or version-wanted help-wanted)
        (begin (when version-wanted
                 (display (string-append "gwwm " %version "\n")))
               (when help-wanted
                 (display (G_ "\
gwwm [OPTION]
  -v --version  Display version
  -h --help     Display this help
  -l --load=FILE     load FILE after gwwm init
  -e --eval=EXPR     eval EXPR after gwwm init
")))
               (exit 0)))))
(define-once global-keymap
  (make-parameter (make-keymap)))
(define (setup-server)
  (or (false-if-exception (and (spawn-server)
                               (add-hook! gwwm-cleanup-hook stop-server-and-clients! )))
      (send-log DEBUG (G_ "repl setup fail."))))
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
  (gwwm-config (load-init-file)))

(define (closemon m)
  (assume (is-a? m <gwwm-monitor>))
  (for-each (lambda (c)
              (let ((geom (client-geom c)))
                (when (and (client-floating? c)
                           (> (box-x geom)
                              (box-width (monitor-area m))))
                  (client-resize c (modify-instance geom
                                     (x (- x (box-width (monitor-window-area m))))) #t))
                (when (equal? (client-monitor c) m)
                  (setmon
                   c
                   (current-monitor)))))
            (client-list)))

(define-once background-layer #f)
(define-once bottom-layer     #f)
(define-once tile-layer       #f)
(define-once fullscreen-layer #f)
(define-once float-layer      #f)
(define-once top-layer        #f)
(define-once overlay-layer    #f)
(define-once no-focus-layer   #f)

(define (surface-notify-enter s kb)
  (wlr-seat-keyboard-notify-enter
   (gwwm-seat)
   s
   (and kb (.keycodes kb) )
   (if kb (.num-keycodes kb) 0)
   (and kb(.modifiers kb))))

(define-public (focusclient c lift)
  (let/ec return
    (unless (exclusive-focus)
      (if c
          (let ((old (~ (gwwm-seat) 'keyboard-state 'focused-surface)))
            (unless (equal? (client-surface c) old)
              (when lift
                (client-scene-raise-to-top c))
              (when (is-a? c <gwwm-client>)
                (set! (current-monitor) (client-monitor c))
                (set! (client-urgent? c) 0)
                (client-restack-surface c))
              (when old
                (or
                 (and-let* ((l (wlr-layer-surface-v1-try-from-wlr-surface old)))
                   (if (and (.mapped old)
                            (member (~ l 'current 'layer) '(2 3)))
                       (return)))
                 (activate-surface old #f)))
              (surface-notify-enter (client-surface c)
                                    (wlr-seat-get-keyboard (gwwm-seat)))
              (activate-surface (client-surface c) #t)))
          (wlr-seat-keyboard-notify-clear-focus (gwwm-seat))))))

(define (setmon c m)
  (pk 'setmon)
  (let ((old (client-monitor c))
        (surface (client-surface c)))
    (unless (equal? old m)
      (run-hook client-set-monitor-hook c old m)
      (set! (client-monitor c) m)
      (when old
        (wlr-surface-send-leave
         surface
         (monitor-output old))
        (arrange old))
      (when m
        (wlr-surface-send-enter surface (monitor-output m))
        (arrange m))
      (focusclient (focustop (current-monitor)) #t))))

(define (arrange-interactive-layer m)
  (send-log DEBUG "arrange-interactive-layer" 'm m)
  (let* ((layers (slot-ref m 'layers)))
    (any (lambda (c)
           (let* ((surface (client-surface c))
                  (lsurface (client-super-surface c)))
             (if (= 1 (~ lsurface 'current 'keyboard-interactive))
                 (begin (focusclient #f #f)
                        (exclusive-focus surface)
                        (surface-notify-enter surface (wlr-seat-get-keyboard (gwwm-seat)))
                        #t)
                 #f)))
         (append (car (list-ref layers 3)) (car (list-ref layers 2))))))

(define (arrangelayer m q-list box exclusive?)
  (define (arrange-layer-client c)
    (let ((exclusive-zone
           (.exclusive-zone
            (.current (client-super-surface c)))))
      (when (eq? exclusive? (> exclusive-zone 0) )
        (wlr-scene-layer-surface-v1-configure
         (slot-ref c 'scene-layer-surface)
         (monitor-area m)
         box)
        (modify-instance* (client-geom c)
          (x (.x (.node (client-scene c))))
          (y (.y (.node (client-scene c)))))
        (client-scene-move
         c
         (box-x (client-geom c))
         (box-y (client-geom c))))))
  (for-each arrange-layer-client (car q-list)))

(define-public (arrangelayers m)
  (pk 'arrangelayers)
  (let* ((l (reverse (slot-ref m 'layers)))
         (box (shallow-clone (monitor-area m))))
    (for-each (cut arrangelayer m <> box #t) l)
    (unless (equal? box (monitor-window-area m))
      (pk box)
      (set! (monitor-window-area m) (shallow-clone box))
      (arrange m))
    (for-each (cut arrangelayer m <> box #f) l)
    (arrange-interactive-layer m)))

(define (update-monitor m config)
  (pk 'update-monitor)
  (let* ((output (monitor-output m))
         (config-head (wlr-output-configuration-head-v1-create config output))
         (box (wlr-output-layout-get-box (gwwm-output-layout) output))
         (state (.state config-head)))
    (set! (monitor-area m) box)
    (set! (monitor-window-area m) (shallow-clone box))
    (and=> (monitor-scene-output m)
           (cut wlr-scene-output-set-position
                <>
                (box-x box)
                (box-y box)))
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
               ((.enabled (monitor-output m))))
      (for-each (lambda (c)
                  (when (and (not (client-monitor c)) (client-mapped? c))
                    (setmon c m))) (client-list)))
    (wlr-output-manager-v1-set-configuration (gwwm-output-manager) config)))

(define (create-keyboard device)
  (send-log INFO "new keyboard create" 'device device)
  (let* ((wl-kb (wlr-keyboard-from-input-device device))
         (kb (make <gwwm-keyboard> #:device device))
         (context (xkb-context-new XKB_CONTEXT_NO_FLAGS))
         (xkb-rule-names
          (xkb-rules))
         (keymap (xkb-keymap-new
                  context
                  xkb-rule-names
                  #:flags
                  XKB_KEYMAP_COMPILE_NO_FLAGS)))
    (wlr-keyboard-set-keymap wl-kb keymap)
    (keyboard-set-repeat-info kb (repeat-rate) 600)
    (run-hook create-keyboard-hook kb)
    (add-listen wl-kb 'modifiers
                (lambda (listener wlr-keyboard)
                  (let* ((modifiers(.modifiers wlr-keyboard)))
                    (wlr-seat-set-keyboard
                     (gwwm-seat) wl-kb)
                    (run-hook modifiers-event-hook kb)
                    (wlr-seat-keyboard-notify-modifiers
                     (gwwm-seat) modifiers)))
                #:destroy-when device)
    (add-listen wl-kb 'key
                (lambda (listener event)
                  (let* ((seat (gwwm-seat))
                         (keybinding (@@ (gwwm keybind) keybinding)))
                    (run-hook keypress-event-hook kb event)
                    (unless (and (not (.active-inhibitor
                                       (gwwm-input-inhibit-manager)))
                                 (keybinding
                                  (wlr-keyboard-get-modifiers wl-kb)
                                  (+ 8 (.keycode event))
                                  (eq? (.state event) 'WL_KEYBOARD_KEY_STATE_PRESSED)))
                      (wlr-seat-set-keyboard seat wl-kb)
                      (wlr-seat-keyboard-notify-key
                       seat
                       (.time-msec event)
                       (.keycode event)
                       (if (eq? (.state event) 'WL_KEYBOARD_KEY_STATE_PRESSED)
                           1 0)))))
                #:destroy-when device)))
(define (request-start-drag listener event)
  (send-log DEBUG "request start drag")
  (let* ((seat (~ event 'drag 'seat)))
    (if (wlr-seat-validate-pointer-grab-serial
         seat
         (.origin event)
         (.serial event))
        (wlr-seat-start-pointer-drag seat (.drag event) (.serial event))
        (wlr-data-source-destroy (~ event 'drag 'source)))))

(define* (motionnotify #:optional (time 0))
  (when (zero? time)
    (idle-activity))
  (when (sloppyfocus?)
    (set! (current-monitor) (monitor-at (.x (gwwm-cursor)) (.y (gwwm-cursor)))))
  (run-hook motion-notify-hook time)
  (let ((cursor (gwwm-cursor)))
    (case (cursor-mode)
      ((move) (client-resize
               (grabc)
               (make <wlr-box>
                 #:x (inexact->exact (round (- (.x cursor) (grabcx))))
                 #:y (inexact->exact (round (- (.y cursor) (grabcy))))
                 #:width (box-width (client-geom (grabc)))
                 #:height (box-height (client-geom (grabc))))
               #t))
      ((resize) (client-resize
                 (grabc)
                 (make <wlr-box>
                   #:x (box-x (client-geom (grabc)))
                   #:y (box-y (client-geom (grabc)))
                   #:width
                   (inexact->exact
                    (round (- (.x cursor)
                              (box-x (client-geom (grabc))))))
                   #:height
                   (inexact->exact
                    (round (- (.y cursor)
                              (box-y (client-geom (grabc)))))))

                 #t))
      ((normal)
       (maybe-let*-values
        (((c surface sx sy) (client-at cursor)))
        (when (and (not surface) (zero? time))
          (wlr-cursor-set-xcursor
           (gwwm-cursor)
           (gwwm-xcursor-manager)
           (cursor-normal-image)))
        (pointerfocus c surface sx sy time))))))

(define (idle-activity . _)
  (wlr-idle-notifier-v1-notify-activity (gwwm-idle) (gwwm-seat)))

(define (cursor-setup)
  (define ((cursor/button cursor) listener event)
    (let* ((pressed (eq? (.state event) 'WLR_BUTTON_PRESSED)))
      (idle-activity)
      (run-hook cursor-button-event-hook event)
      (case (cursor-mode)
        ((normal)
         (maybe-let*-values
          (((c scene-obj sx sy) (client-at cursor)))
          (focusclient c #t))
         (let* ((keyboard (wlr-seat-get-keyboard (gwwm-seat)))
                (mods (if keyboard (wlr-keyboard-get-modifiers
                                    keyboard) 0)))
           (unless ((@@ (gwwm keybind) keybinding)
                    mods
                    (+ 8 (.button event))
                    pressed)
             (wlr-seat-pointer-notify-button
              (gwwm-seat)
              (.time-msec event)
              (.button event)
              (.state event)))))
        (else => (lambda (o)
                   (unless pressed
                     (and-let* (((eq? o 'resize))
                                (c (grabc)))
                       (client-set-resizing! c #f))
                     (wlr-cursor-set-xcursor
                      cursor
                      (gwwm-xcursor-manager)
                      (cursor-normal-image))
                     (cursor-mode 'normal)
                     (set! (current-monitor)
                           (monitor-at (.x cursor) (.y cursor)))
                     (setmon (grabc) (current-monitor))))))))

  (let ((cursor (gwwm-cursor (wlr-cursor-create))))
    (add-listen cursor 'touch-up
                (lambda (listener event)
                  (let-slots event (touch time-msec touch-id)
                    (send-log INFO "touch-up"
                              'touch touch
                              'time-msec time-msec
                              'touch-id touch-id)
                    (wlr-seat-touch-notify-up (gwwm-seat) time-msec touch-id)
                    (wlr-seat-touch-point-clear-focus (gwwm-seat) time-msec touch-id)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'touch-down
                (lambda (listener event)
                  (let-slots event (touch time-msec touch-id x y)
                    (send-log INFO "touch-down"
                              'touch touch
                              'time-msec time-msec
                              'touch-id touch-id
                              'x x
                              'y y)
                    (let* ((o (wlr-cursor-absolute-to-layout-coords
                               (gwwm-cursor)
                               (.base touch)
                               x
                               y)))
                      (maybe-let*-values
                       (((c surface sx sy) (client-at (car o) (cdr o))))
                       (when (wlr-surface? surface)
                         (wlr-seat-touch-point-focus
                          (gwwm-seat)
                          surface
                          time-msec touch-id sx sy)
                         (wlr-seat-touch-notify-down
                          (gwwm-seat)
                          surface
                          time-msec touch-id sx sy))))))
                #:remove-when-destroy? #f)
    (add-listen cursor 'touch-motion
                (lambda (listener event)
                  (let-slots event (touch time-msec touch-id x y)
                    (let ((o (wlr-cursor-absolute-to-layout-coords
                              (gwwm-cursor)
                              (.base touch)
                              x
                              y)))
                      (maybe-let*-values
                       (((c surface sx sy) (client-at (car o) (cdr o))))
                       (wlr-seat-touch-notify-motion (gwwm-seat) time-msec touch-id sx sy)))))
                #:remove-when-destroy? #f)
    (add-listen cursor 'touch-cancel
                (lambda (listener event)
                  (let-slots event (touch time-msec touch-id)
                    (send-log INFO "touch-cancel"
                              'touch touch
                              'time-msec time-msec
                              'touch-id touch-id)
                    (maybe-let*-values
                     (((c surface sx sy) (client-at (gwwm-cursor))))
                     (when (wlr-surface? surface)
                       (wlr-seat-touch-notify-cancel
                        (gwwm-seat)
                        surface)))))
                #:remove-when-destroy? #f)
    (add-listen cursor 'touch-frame
                (lambda (listener data)
                  ;; (send-log INFO "touch-frame" )
                  (wlr-seat-touch-notify-frame (gwwm-seat)))
                #:remove-when-destroy? #f)

    (add-listen cursor 'axis
                (lambda (listener event)
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
                  (idle-activity))
                #:remove-when-destroy? #f)
    (add-listen cursor 'swipe-begin
                (lambda (listener event)
                  (let-slots event (pointer time-msec fingers)
                    (send-log INFO "swipe-begin"
                              'pointer pointer
                              'time-msec time-msec
                              'fingers fingers)
                    (wlr-pointer-gestures-v1-send-swipe-begin
                     (gwwm-pointer-gestures)
                     (gwwm-seat)
                     time-msec
                     fingers)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'swipe-update
                (lambda (listener event)
                  (let-slots event (pointer time-msec fingers dx dy)
                    (wlr-pointer-gestures-v1-send-swipe-update
                     (gwwm-pointer-gestures)
                     (gwwm-seat)
                     fingers
                     dx
                     dy)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'swipe-end
                (lambda (listener event)
                  (let-slots event (pointer time-msec cancelled)
                    (send-log INFO "swipe-end"
                              'pointer pointer
                              'time-msec time-msec
                              'cancelled cancelled)
                    (wlr-pointer-gestures-v1-send-swipe-end
                     (gwwm-pointer-gestures)
                     (gwwm-seat)
                     time-msec
                     cancelled)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'pinch-begin
                (lambda (listener event)
                  (let-slots event (pointer time-msec fingers)
                    (send-log INFO "pinch-begin"
                              'pointer pointer
                              'time-msec time-msec
                              'fingers fingers )
                    (wlr-pointer-gestures-v1-send-pinch-begin
                     (gwwm-pointer-gestures)
                     (gwwm-seat)
                     time-msec
                     fingers)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'pinch-update
                (lambda (listener event)
                  (let-slots event (pointer time-msec fingers dx dy scale rotation)
                    (wlr-pointer-gestures-v1-send-pinch-update
                     (gwwm-pointer-gestures)
                     (gwwm-seat)
                     fingers
                     dx
                     dy
                     scale
                     rotation)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'pinch-end
                (lambda (listener event)
                  (let-slots event (pointer time-msec cancelled)
                    (send-log INFO "pinch-end"
                              'pointer pointer
                              'time-msec time-msec
                              'cancelled cancelled)
                    (wlr-pointer-gestures-v1-send-pinch-end
                     (gwwm-pointer-gestures)
                     (gwwm-seat)
                     time-msec
                     cancelled)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'hold-begin
                (lambda (listener event)
                  (let-slots event (pointer time-msec fingers)
                    (send-log INFO "hold-begin"
                              'pointer pointer
                              'time-msec time-msec
                              'fingers fingers )
                    (wlr-pointer-gestures-v1-send-hold-begin
                     (gwwm-pointer-gestures)
                     (gwwm-seat)
                     time-msec
                     fingers)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'hold-end
                (lambda (listener event)
                  (let-slots event (pointer time-msec cancelled)
                    (send-log INFO "hold-end"
                              'pointer pointer
                              'time-msec time-msec
                              'cancelled cancelled)
                    (wlr-pointer-gestures-v1-send-hold-end
                     (gwwm-pointer-gestures)
                     (gwwm-seat)
                     time-msec
                     cancelled)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'frame
                (lambda (listener cursor)
                  (run-hook cursor-frame-event-hook cursor)
                  (wlr-seat-pointer-notify-frame (gwwm-seat)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'motion
                (lambda (listener event)
                  (wlr-cursor-move (gwwm-cursor)
                                   (.base (.pointer event))
                                   (.delta-x event)
                                   (.delta-y event))
                  (motionnotify (.time-msec event)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'motion-absolute
                (lambda (listener event)
                  (let-slots event (pointer x y time-msec)
                    (wlr-cursor-warp-absolute
                     (gwwm-cursor)
                     (.base pointer) x y)
                    (motionnotify time-msec)))
                #:remove-when-destroy? #f)
    (add-listen cursor 'button (cursor/button cursor)
                #:remove-when-destroy? #f)))

(define (activate-surface surface activate?)
  ;; (assert (wlr-surface? surface))
  (let ((super-surface (super-surface-try-from-wlr-surface surface)))
    (cond ((and
            (wlr-xdg-surface? super-surface)
            (eq? (.role super-surface)
                 'WLR_XDG_SURFACE_ROLE_TOPLEVEL))

           (wlr-xdg-toplevel-set-activated
            (wlr-xdg-surface-toplevel super-surface)
            activate? )))))

(define (seat-setup display)
  (let ((seat (gwwm-seat (wlr-seat-create (gwwm-display) "seat0"))))
    (add-listen seat 'request-set-cursor
                (lambda (listener event)
                  (when (eq? (cursor-mode) 'normal)
                    (let-slots event (seat-client surface hostpot-x hostpot-y)
                      (when (equal? seat-client
                                    (~ seat 'pointer-state 'focused-client))
                        (wlr-cursor-set-surface (gwwm-cursor)
                                                surface
                                                hostpot-x
                                                hostpot-y))))))
    (add-listen seat 'request-set-selection
                (lambda (listener event)
                  (run-hook selection-hook event)
                  (wlr-seat-set-selection seat (.source event)
                                          (.serial event))))
    (add-listen seat 'request-start-drag request-start-drag)
    (add-listen seat 'request-set-primary-selection
                (lambda (listener event)
                  (send-log INFO "seat request set primary selection")
                  (wlr-seat-set-primary-selection seat (.source event) (.serial event))))
    (add-listen (.keyboard-state seat) 'focus-change
                (lambda (listener event)
                  (let* ((old-client (and=> (.old-surface event)
                                            client-from-wlr-surface))
                         (new-client (and=> (.new-surface event)
                                            client-from-wlr-surface)))

                    (run-hook keyboard-focus-change-hook
                              seat old-client new-client)))
                #:destroy-when seat)
    (add-listen seat 'start-drag
                (lambda (listener drag)
                  (and-let* (drag
                             (icon (.icon drag))
                             (scene (wlr-scene-subsurface-tree-create
                                     no-focus-layer
                                     (.surface icon)))
                             (drag-move
                              (lambda _
                                (wlr-scene-node-set-position
                                 (.node scene)
                                 (inexact->exact
                                  (round (+ (.x (gwwm-cursor))
                                            (.dx (.current (.surface icon))))))
                                 (inexact->exact
                                  (round (+ (.y (gwwm-cursor))
                                            (.dy (.current (.surface icon))))))))))

                    (add-hook! motion-notify-hook drag-move)
                    (motionnotify)
                    (add-listen icon 'destroy
                                (lambda (listener data)
                                  (remove-hook! motion-notify-hook drag-move)
                                  (wlr-scene-node-destroy (.node scene))
                                  (focusclient (current-client) #t)
                                  (motionnotify))
                                #:remove-when-destroy? #f))))))


(define non-zero? (negate zero?))
(define (create-pointer device)
  (define p (make <gwwm-pointer> #:device device))
  (and-let* (((wlr-input-device-is-libinput device))
             (libinput-device (wlr-libinput-get-device-handle device)))
    (when (non-zero? (libinput-device-config-tap-get-finger-count libinput-device))
      (libinput-device-config-tap-set-enabled
       libinput-device 'LIBINPUT_CONFIG_TAP_ENABLED)
      (libinput-device-config-tap-set-drag-enabled
       libinput-device 'LIBINPUT_CONFIG_DRAG_ENABLED)
      (libinput-device-config-tap-set-drag-lock-enabled
       libinput-device 'LIBINPUT_CONFIG_DRAG_LOCK_ENABLED))

    (set! (pointer-natural-scroll? p) #t)
    (set! (pointer-disable-while-typing? p) #t)
    (set! (pointer-left-handed? p) #f)
    (set! (pointer-middle-emulation? p) #f)

    (unless (= (libinput-device-config-scroll-get-methods libinput-device) 0)
      (libinput-device-config-scroll-set-method libinput-device 'LIBINPUT_CONFIG_SCROLL_2FG))
    (unless (= (libinput-device-config-click-get-methods libinput-device) 0)
      (libinput-device-config-click-set-method libinput-device 'LIBINPUT_CONFIG_CLICK_METHOD_BUTTON_AREAS))
    (when (non-zero? (libinput-device-config-send-events-get-modes libinput-device ))
      (libinput-device-config-send-events-set-mode libinput-device
                                                   LIBINPUT_CONFIG_SEND_EVENTS_ENABLED))
    (when (libinput-device-config-accel-is-available libinput-device)
      (libinput-device-config-accel-set-profile libinput-device 'LIBINPUT_CONFIG_ACCEL_PROFILE_ADAPTIVE)
      (libinput-device-config-accel-set-speed libinput-device 0.0)))
  (wlr-cursor-attach-input-device (gwwm-cursor) device))

(define (init-monitor m)
  (assert (is-a? m <gwwm-monitor>))
  (let* ((name (monitor-name m))
         (rule (or (find (lambda (x) (equal? x name)) (monitor-rules))
                   (default-monitor-rules)))
         (output (monitor-output m))
         (scale (.scale rule)))
    (wlr-output-set-scale output scale)
    (wlr-xcursor-manager-load (gwwm-xcursor-manager) scale)
    (wlr-output-set-transform output (.reflect rule))))

(define (draw-monitor-cairo cr m)
  (define font-size 24)
  (define* (draw-newline text)
    (let ((x y (cairo-get-current-point cr)))
      (cairo-move-to cr 5 (+ y font-size))
      (cairo-show-text cr text)))
  (define target (cairo-get-target cr))
  (cairo-select-font-face cr "" 'normal 'bold)
  (cairo-set-font-size cr 24)
  (cairo-set-source-rgba cr 1.0 0.0 0.0 0.8)
  (cairo-set-fill-rule cr 'winding)

  (draw-newline (monitor-name m))
  (cairo-fill cr)
  (cairo-surface-flush target))

(define (backend-setup display)
  (define (add-seat-capabilitie seat o)
    (wlr-seat-set-capabilities seat
                               (logior (.capabilities seat)
                                       (bs:enum->integer
                                        %wl-seat-capability-enum o))))
  (define (backend/new-output listener wlr-output)
    (let* ((m (make <gwwm-monitor>
                #:wlr-output wlr-output
                #:layouts (make-list 2 tile-layout))))
      (set! (wlr-output->monitor wlr-output) m)
      (init-monitor m)

      (when (wlr-output-init-render wlr-output (gwwm-allocator) (gwwm-renderer))

        (let ((state (make <wlr-output-state>))
              (preferred-mode (wlr-output-preferred-mode wlr-output)))
          (wlr-output-state-set-mode state preferred-mode)
          (unless (wlr-output-test-state wlr-output state)
            (send-log DEBUG "preferred mode rejected, try fallback to another mode"
                      'mode
                      preferred-mode)
            (let ((success (find (lambda (mode)
                                   (unless (equal? mode preferred-mode)
                                     (wlr-output-state-set-mode state mode)
                                     (wlr-output-test-state wlr-output state)))
                                 (wl-list->list (.modes wlr-output)
                                                <wlr-output-mode>
                                                'link))))
              (apply send-log DEBUG
                     (if success
                         (list "fallback to" 'mode success)
                         (list "fallback fail!")))))
          (wlr-output-commit-state wlr-output state)
          (wlr-output-state-set-enabled state #t)
          (send-log INFO "enable adaptive-sync" 'monitor m)
          (wlr-output-state-set-adaptive-sync-enabled state #t)
          (unless (wlr-output-test-state wlr-output state)
            (send-log INFO "enable adaptive-sync fail" 'monitor m))
          (begin
            (unless (wlr-output-commit-state wlr-output state)
              (send-log WARNING "wlr-output-commit-state fail!"
                        'monitor m
                        'state state))
            (cond ((wlr-output-is-wl wlr-output)
                   (wlr-wl-output-set-title wlr-output "gwwm/wayland")))
            (q-push! (%monitors) m)
            (let ((lo (wlr-output-layout-add-auto (gwwm-output-layout) wlr-output))
                  (scene-output (wlr-scene-output-create (gwwm-scene) wlr-output)))

              (wlr-scene-output-layout-add-output
               (gwwm-scene-output-layout)
               lo
               scene-output)
              (set! (monitor-scene-output m) scene-output)
              (add-listen scene-output 'destroy
                          (lambda _ (set! (monitor-scene-output m) #f))))

            (add-listen (monitor-output m) 'frame render-monitor)
            (add-listen (monitor-output m) 'request-state
                        (lambda (listener event)
                          (send-log INFO "request-state")
                          (wlr-output-commit-state wlr-output (.state event))))
            (add-listen (monitor-output m) 'destroy cleanup-monitor)
            (wlr-output-commit-state wlr-output  state)
            (when (getenv "GWWM_DEBUG")
              (let* ((area (monitor-area m))
                     (buf (cairo-buffer-create (box-width area)
                                               (box-height area)))
                     (cr (cairo-buffer-cairo buf)))
                (draw-monitor-cairo cr m)
                (letrec* ((buffer (wlr-scene-buffer-create
                                   overlay-layer
                                   (cairo-buffer-base buf)))
                          (node (.node buffer))
                          (listener (make-wl-listener
                                     (lambda _
                                       (wlr-buffer-drop (cairo-buffer-base buf))
                                       (wl-listener-remove listener)))))
                  (wl-signal-add (get-event-signal node 'destroy)
                                 listener)
                  (let ((area (monitor-window-area m)))
                    (wlr-scene-node-set-position node
                                                 (box-x area)
                                                 (box-y area))))))
            (run-hook create-monitor-hook m)
            (wlr-output-state-finish state))))))

  (define (backend/new-input listener device)
    (case (.type device)
      ((WLR_INPUT_DEVICE_KEYBOARD)
       (create-keyboard device)
       (unless (null? (keyboard-list))

         (add-seat-capabilitie
          (gwwm-seat)
          'WL_SEAT_CAPABILITY_KEYBOARD)))
      ((WLR_INPUT_DEVICE_POINTER)
       (create-pointer device)
       (add-seat-capabilitie
        (gwwm-seat)
        'WL_SEAT_CAPABILITY_POINTER))
      ((WLR_INPUT_DEVICE_TOUCH)
       (make <gwwm-touch> #:device device)
       (wlr-cursor-attach-input-device (gwwm-cursor) device)
       (add-seat-capabilitie
        (gwwm-seat)
        'WL_SEAT_CAPABILITY_TOUCH))
      ((WLR_INPUT_DEVICE_SWITCH)
       (send-log WARNING "TODO"))
      ((WLR_INPUT_DEVICE_TABLET_TOOL)
       (send-log WARNING "TODO"))
      ((WLR_INPUT_DEVICE_TABLET_PAD)
       (send-log WARNING "TODO"))
      (else (send-log WARNING "unknow input device"))))
  (let* ((backend session (wlr-backend-autocreate(gwwm-display))))
    (unless backend
      (send-log ERROR (G_ "gwwm Couldn't create backend"))
      (exit 1))
    (when (and (getenv "GWWM_DEBUG") (wlr-backend-is-multi backend))
      (wlr-multi-for-each-backend
       backend (lambda (backend)
                 (cond ((wlr-backend-is-wl backend)
                        (wlr-wl-output-create backend))))))
    (and=> backend gwwm-backend)
    (and=> session gwwm-session)
    (add-listen backend 'new-input backend/new-input)
    (add-listen backend 'new-output backend/new-output)))

(define-syntax-rule (begin0 body0 body ...)
  (let ((a body0))
    body
    ...
    a))

(define (output-manager-apply-or-test config test?)
  (let ((heads (wl-list->list (.heads config)
                              <wlr-output-configuration-head-v1>
                              'link)))
    (if (every (lambda (obj)
                 (let* ((state (.state obj))
                        (output (.output state))
                        (mode (.mode state))
                        (is-output (.enabled state))
                        (scale (.scale state)))
                   (wlr-output-enable output is-output)
                   (if is-output
                       (wlr-output-layout-add (gwwm-output-layout)
                                              output
                                              (.x state)
                                              (.y state))
                       (wlr-output-layout-remove (gwwm-output-layout)
                                                 output))
                   (if mode
                       (wlr-output-set-mode output mode)
                       (send-log
                        WARNING
                        (G_ "for now, not support custom mode" )))
                   (wlr-output-set-transform output (.transform state))
                   (when (> scale 0)
                     (wlr-output-set-scale output scale))
                   (if test?
                       (begin0
                        (wlr-output-test output)
                        (wlr-output-rollback output))
                       (wlr-output-commit output))))
               heads)
        (wlr-output-configuration-v1-send-succeeded config)
        (wlr-output-configuration-v1-send-failed config))
    )

  (wlr-output-configuration-v1-destroy config))
(define (output-manager-setup display)
  (let ((output-manager (gwwm-output-manager (wlr-output-manager-v1-create display))))


    (add-listen output-manager 'apply
                (lambda (listener config)
                  (output-manager-apply-or-test config #f)))
    (add-listen output-manager 'test
                (lambda (listener config)
                  (output-manager-apply-or-test config #t)))))

(define (return-scene-node n)
  (case n
    ((0) background-layer)
    ((1) bottom-layer)
    ((2) top-layer)
    ((3) overlay-layer)))

(define (gwwm-setup)
  (gwwm-display (wl-display-create))
  (backend-setup (gwwm-display))
  (or (and=> (wlr-renderer-autocreate (gwwm-backend)) gwwm-renderer)
      (begin (send-log ERROR (G_ "gwwm Couldn't create renderer"))
             (exit 1)))
  (wlr-renderer-init-wl-display (gwwm-renderer) (gwwm-display))
  (or (and=> (wlr-allocator-autocreate
              (gwwm-backend)
              (gwwm-renderer)) gwwm-allocator)
      (begin (send-log ERROR (G_ "gwwm Couldn't create allocator"))
             (exit 1)))
  (cursor-setup)

  (gwwm-xcursor-manager (wlr-xcursor-manager-create #f 24))
  (seat-setup (gwwm-display))
  (gwwm-pointer-gestures (wlr-pointer-gestures-v1-create (gwwm-display)))
  (gwwm-xdg-shell (wlr-xdg-shell-create (gwwm-display) 5))
  (add-listen (gwwm-xdg-shell) 'new-surface
              (lambda (listener xdg-surface)
                (when (eq? (.role xdg-surface)
                           'WLR_XDG_SURFACE_ROLE_TOPLEVEL)
                  (let* ((scene
                          (wlr-scene-xdg-surface-create tile-layer
                                                        xdg-surface))
                         (c (make <gwwm-xdg-client>
                              #:scene scene
                              #:super-surface xdg-surface)))
                    (set! (client-border-width c) (borderpx))

                    (run-hook create-client-hook c)))))
  (gwwm-compositor (wlr-compositor-create (gwwm-display) 5 (gwwm-renderer)))
  (wlr-subcompositor-create (gwwm-display))
  (gwwm-activation (wlr-xdg-activation-v1-create (gwwm-display)))
  (add-listen (gwwm-activation) 'request-activate
              (lambda (listener event)
                (and-let* ((c (client-from-wlr-surface (.surface event))))
                  (unless (equal? c (current-client))
                    (set! (client-urgent? c) #t)))))
  (gwwm-layer-shell (wlr-layer-shell-v1-create (gwwm-display) 4))
  (add-listen (gwwm-layer-shell) 'new-surface
              (lambda (listener layer-surface)
                (unless (.output layer-surface)
                  (set! (.output layer-surface)
                        (monitor-output (current-monitor))))
                (let* ((scene-surface (wlr-scene-layer-surface-v1-create
                                       (pk 'ret
                                           (return-scene-node
                                            (~ layer-surface 'pending 'layer)))
                                       layer-surface))
                       (scene  (.tree scene-surface))
                       (c (make <gwwm-layer-client>
                            #:super-surface layer-surface
                            #:scene scene
                            #:monitor (wlr-output->monitor (.output layer-surface))
                            #:scene-layer-surface scene-surface)))
                  (set! (surface->scene (client-surface c)) (client-scene c))
                  (set! (scene-node->client (.node (client-scene c))) c)
                  (q-push! (list-ref (slot-ref (client-monitor c) 'layers)
                                     (~ layer-surface 'pending 'layer))
                           c)
                  (run-hook create-client-hook c))))
  (gwwm-idle (wlr-idle-notifier-v1-create (gwwm-display)))
  (gwwm-output-layout (wlr-output-layout-create))
  (add-listen (gwwm-output-layout) 'change update-monitors)
  (add-hook! keypress-event-hook idle-activity)
  (add-hook! client-set-monitor-hook
             (lambda (c old new)
               (when (and new (not (is-a? c <gwwm-layer-client>)))
                 (set! (client-tags c)
                       (or (and=> (client-get-parent c) client-tags)
                           (list-ref (slot-ref new 'tagset)
                                     (slot-ref new 'seltags)))))))
  (output-manager-setup (gwwm-display))
  (wlr-cursor-attach-output-layout (gwwm-cursor) (gwwm-output-layout)))


(define ((unmap-notify* c) listener data)
  (send-log DEBUG (G_ "Client unmapping")  'client c)
  (when (equal? c (grabc))
    (cursor-mode 'normal)
    (grabc #f))
  (setmon c #f)
  (q-remove! (%clients) c)
  (q-remove! (%fstack) c))

(define-method (client-init-geom (c <gwwm-client>))
  (let ((geom (client-get-geometry c)))
    (send-log DEBUG (G_ "Client init geom")  'client c 'geom geom)
    (set! (client-geom c) geom)
    (set! (client-prev-geom c) (shallow-clone geom))
    (send-log DEBUG (G_ "Client init geom done")  'client c 'geom geom)))

(define (map-notify* c)
  (lambda (listener data)
    (define surface (client-surface c))
    (send-log DEBUG (G_ "Client mapping")  'client c 'c surface)
    (assert (and (wlr-surface? surface) surface "map-notify*"))
    (client-init-geom c)
    (client-init-border c)
    (run-hook client-map-event-hook c
              (super-surface-try-from-wlr-surface surface))
    (set! (super-surface->scene (client-super-surface c)) (client-scene c))
    (set! (scene-node->client (.node (client-scene c))) c)
    (set! (surface->scene (client-surface c)) (client-scene c))

    (add-listen (client-surface c) 'commit (client-commit-notify c))
    (q-push! (%clients) c)
    (q-push! (%fstack) c)
    (let ((parent (client-get-parent c)))
      (send-log DEBUG "client's parent" 'client c 'parent parent)
      (if parent
          (begin (setmon c (or (client-monitor parent)
                               (current-monitor)))
                 (client-do-set-floating c #t))
          (begin
            (setmon c (current-monitor))
            (client-do-set-floating c (client-is-float-type? c)))))
    (client-do-set-fullscreen c )))

(define ((map-layer-client-notify c) listener data)
  (send-log DEBUG "layer client map" 'client c)
  (wlr-surface-send-enter
   (client-surface c)
   (monitor-output(client-monitor c)))
  (arrangelayers (client-monitor c))
  (motionnotify))
(define ((unmap-layer-client-notify c) listener data)
  (send-log DEBUG "layer client unmap" 'client c)
  (define surface (client-surface c))
  (when (equal? surface (exclusive-focus))
    (exclusive-focus #f))
  (when (equal? surface
                (.focused-surface
                 (.keyboard-state (gwwm-seat))))
    (focusclient (current-client) #f))
  (arrangelayers (client-monitor c))
  (motionnotify)
  (send-log DEBUG "layer client unmap" 'client c))

(define (render-monitor listener output)
  (and-let* ((m (wlr-output->monitor output))
             (scene-output (monitor-scene-output m)))
    (let ((_ now (clock-gettime 1)))
      (when (wlr-scene-output-commit scene-output)
        (wlr-scene-output-send-frame-done scene-output now)))))

(define (cleanup-monitor listener wlr-output)
  (assert (wlr-output? wlr-output))
  (define m (wlr-output->monitor wlr-output))
  (q-remove! (%monitors) m)
  (wlr-output-layout-remove (gwwm-output-layout) (monitor-output m))
  (set! (monitor-scene-output m) #f)
  (let* ((ms (monitor-list))
         (l (length ms)))
    (if (zero? l)
        (when (quit-when-no-monitor?)
          (gwwm-quit))
        (let  ((m (find (lambda (m)
                          (.enabled (monitor-output m))) ms)))
          (when m
            (set! (current-monitor) m)))))
  (focusclient (focustop (current-monitor)) #t)
  (closemon m))

(define (scene-setup display backend)
  (let ((scene (gwwm-scene (wlr-scene-create))))
    (send-log INFO "scene-setup")
    (wlr-scene-set-presentation scene (wlr-presentation-create display backend))
    (let ((create (lambda () (wlr-scene-tree-create (.tree scene)))))
      (set! background-layer (create))
      (set! bottom-layer (create))
      (set! tile-layer (create))
      (set! fullscreen-layer (create))
      (set! float-layer (create))
      (set! top-layer (create))
      (set! overlay-layer (create))
      (set! no-focus-layer (create)))
    (send-log INFO "scene-setup-done")))

(define (pointerfocus c surface sx sy time)
  (let ((internal-call (not time)))
    (when (and c (sloppyfocus?) (not internal-call))
      (focusclient c #f))
    (if surface
        (let* ((_ now (clock-gettime 1))
               (time (+ (* 1000 (.tv-sec now)) (/ (.tv-nsec now) 1000000))))
          (wlr-seat-pointer-notify-enter (gwwm-seat) surface sx sy)
          (wlr-seat-pointer-notify-motion (gwwm-seat)
                                          (if (zero? time)
                                              (let ((_ now (clock-gettime 1)))
                                                (+ (* 1000 (.tv-sec now)) (/ (.tv-nsec now) 1000000)))
                                              (round time))
                                          sx
                                          sy))
        (wlr-seat-pointer-notify-clear-focus (gwwm-seat)))))

(define ((new-popup-notify c) listener popup)
  (send-log DEBUG "client add new popup" 'client c)
  (and-let* (popup
             (scene (surface->scene (.parent popup)))
             (tree (wlr-scene-xdg-surface-create scene (.base popup))))
    (set! (surface->scene (.surface (.base popup))) tree)
    (if (is-a? c <gwwm-layer-client>)
        (wlr-scene-node-reparent (.node tree) (.node top-layer)))
    (run-hook create-popup-hook popup)
    (and-let* ((client-alive? c)
               (client-mapped? c)
               (monitor (client-monitor c))
               (geom (shallow-clone
                      (if (is-a? c <gwwm-layer-client>)
                          (monitor-area monitor)
                          (monitor-window-area (client-monitor c))))))
      (wlr-scene-node-raise-to-top (.node (.parent (.node tree))))
      (modify-instance* geom
                        (x (- x (box-x (client-geom c))))
                        (y (- y (box-y (client-geom c)))))
      (wlr-xdg-popup-unconstrain-from-box popup geom))
    (add-listen (.base popup) 'new-popup (new-popup-notify c))
    (add-listen popup 'reposition
                (lambda (listener data)
                  (send-log DEBUG "popup reposition" 'popup popup 'client c))
                #:destroy-when (.base popup))
    (send-log DEBUG "popup listen 'new-popup" 'popup popup 'client c)))

(define (set-log-callback)
  (current-log-callback
   (let ((p (current-error-port)))
     (lambda (msg)
       (let ((msg2 msg))
         (format p "~a: [~a]| ~a | "
                 (date->string (current-date) "~m-~d ~3 ~N")
                 (match (cdr (assq 'SEVERITY msg))
                   (7 "DEBUG")
                   (6 "INFO")
                   (5 "NOTICE")
                   (4 "WARNING")
                   (3 "ERROR")
                   (2 "CRITICAL")
                   (1 "ALERT")
                   (0 "EMERGENCY")
                   (a a))
                 (cdr (assq 'MESSAGE msg)))
         (set! msg2 (assoc-remove! (assoc-remove! msg2 'SEVERITY) 'MESSAGE))
         (for-each (lambda (a)
                     (display (car a) p)
                     (display ":" p)
                     (display (object->string(cdr a)) p)
                     (display " " p))
                   msg2)
         (newline p))))))

(define (cleanup)
  (run-hook gwwm-cleanup-hook)
  (wl-display-destroy-clients (gwwm-display))
  (wl-display-destroy (gwwm-display)))

(define (main . args)
  (setlocale LC_ALL)
  (textdomain %gettext-domain)
  (install-suspendable-ports!)
  (set-log-callback)
  (add-hook! create-client-hook
             (lambda (c)
               (send-log DEBUG "client createed" 'CLIENT c)))

  (add-hook! keyboard-focus-change-hook
             (lambda (seat old new)
               (when new
                 (q-remove! (%fstack) new)
                 (q-push! (%fstack) new))))
  (add-hook! keyboard-focus-change-hook
             (lambda (seat old new)
               (and=> old
                      (cut client-set-border-color <>
                           (bordercolor)))
               (and=> new
                      (cut client-set-border-color
                           <>
                           (focuscolor)))))
  (add-hook!
   create-client-hook
   (lambda (c)
     (add-listen (client-super-surface c) 'destroy
                 (client-destroy-notify c))
     (when (is-a? c <gwwm-client>)
       (add-listen (client-surface c) 'unmap (unmap-notify* c))
       (add-listen (client-surface c) 'map (map-notify* c)))
     (define (update-appid listener data)
       (let ((new (client-get-appid c)))
         (send-log DEBUG "client update appid"
                   'client c
                   'old (client-appid c))
         (set! (client-appid c) new)))
     (cond ((is-a? c <gwwm-xdg-client>)
            (let* ((super-surface (client-super-surface c))
                   (toplevel (wlr-xdg-surface-toplevel super-surface)))
              (add-listen super-surface 'new-popup
                          (new-popup-notify c))
              (add-listen toplevel 'set-title
                          (client-set-title-notify c)
                          #:destroy-when super-surface)
              (add-listen toplevel 'set-app-id
                          update-appid
                          #:destroy-when super-surface)
              (add-listen toplevel 'request-maximize
                          (lambda (listener data)
                            (send-log DEBUG "client request maximize" 'client c)
                            (wlr-xdg-surface-schedule-configure
                             (client-super-surface c)))
                          #:destroy-when super-surface)
              (add-listen toplevel 'request-fullscreen (client-request-fullscreen-notify c)
                          #:destroy-when super-surface)))
           ((is-a? c <gwwm-layer-client>)
            (add-listen (client-surface c) 'map
                        (map-layer-client-notify c))

            (add-listen (client-super-surface c) 'new-popup
                        (new-popup-notify c))
            (add-listen (.surface (client-super-surface c)) 'commit
                        (lambda (listener data)
                          (and-let* ((m (client-monitor c))
                                     (layer-surface (client-super-surface c)))
                            (unless (equal? (return-scene-node (~ layer-surface 'current 'layer))
                                            (client-scene c))
                              (for-each (cut q-remove! <> c)
                                        (slot-ref m 'layers))
                              (wlr-scene-node-reparent
                               (.node (client-scene c))
                               (.node (return-scene-node
                                       (~ layer-surface 'current 'layer))))
                              (q-push! (list-ref
                                        (slot-ref m 'layers)
                                        (~ layer-surface 'current 'layer))
                                       c))
                            (unless (zero?
                                     (~ layer-surface
                                        'current
                                        'committed))
                              (arrangelayers m)))))
            (add-listen (client-surface c) 'unmap
                        (unmap-layer-client-notify c))))))
  (parse-command-line args)
  (send-log DEBUG (G_ "init global keybind ..."))
  (init-global-keybind)
  (unless (getenv "XDG_RUNTIME_DIR")
    (send-log EMERGENCY (G_ "XDG_RUNTIME_DIR must be set."))
    (exit 1))
  (setvbuf (current-output-port) 'line)
  (setvbuf (current-error-port) 'line)
  (gwwm-setup)
  (scene-setup (gwwm-display) (gwwm-backend))
  (sigaction SIGINT (lambda _
                      (send-log INFO "get SIGINT")
                      (gwwm-quit)))
  (sigaction SIGTERM (lambda _
                       (send-log INFO "get SIGTERM")
                       (gwwm-quit)))
  (wlr-export-dmabuf-manager-v1-create (gwwm-display))
  (wlr-xdg-decoration-manager-v1-create (gwwm-display))
  (wlr-gamma-control-manager-v1-create (gwwm-display))
  (wlr-screencopy-manager-v1-create (gwwm-display))
  (wlr-data-device-manager-create (gwwm-display))
  (let ((dcm (gwwm-data-control-manager (wlr-data-control-manager-v1-create (gwwm-display)))))
    (add-listen dcm 'new-device
                (lambda (listener device)
                  (send-log INFO "new data control device" 'device device))))

  (wlr-primary-selection-v1-device-manager-create (gwwm-display))
  (gwwm-input-inhibit-manager (wlr-input-inhibit-manager-create (gwwm-display)))
  (wlr-viewporter-create (gwwm-display))
  (wlr-xdg-output-manager-v1-create (gwwm-display) (gwwm-output-layout))
  (wlr-server-decoration-manager-set-default-mode
   (wlr-server-decoration-manager-create (gwwm-display))
   (bs:enum->integer %wlr-server-decoration-manager-mode-enum
                     'WLR_SERVER_DECORATION_MANAGER_MODE_SERVER))
  (config-setup)
  (setup-socket)
  ;; wlr-scene-output-layout-add-output
  (gwwm-scene-output-layout (wlr-scene-attach-output-layout (gwwm-scene) (gwwm-output-layout)))
  ;; Start the backend. This will enumerate outputs and inputs, become the DRM
  ;; master, etc
  (if (wlr-backend-start (gwwm-backend))
      (send-log INFO (G_ "backend is started."))
      (begin (send-log ERROR (G_ "gwwm cannot start backend!"))
             (exit 1)))
  (set! (current-monitor) (monitor-at
                           (.x (gwwm-cursor))
                           (.y (gwwm-cursor))))
  (wlr-cursor-warp-closest
   (gwwm-cursor) #f
   (.x (gwwm-cursor))
   (.y (gwwm-cursor)))
  (wlr-cursor-set-xcursor
   (gwwm-cursor)
   (gwwm-xcursor-manager)
   (cursor-normal-image))
  (run-hook gwwm-after-init-hook)
  (set-current-module (resolve-module '(gwwm user)))
  (setup-server)
  (wl-display-run (gwwm-display))
  (cleanup))
