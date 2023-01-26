(define-module (gwwm)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 q)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-189)
  #:use-module (wlroots util box)
  #:use-module (util572 box )
  #:use-module (ice-9 format)
  #:use-module (libinput)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots types input-inhibitor)
  #:use-module (wlroots types data-device)
  #:use-module (wlroots types keyboard)
  #:use-module (wlroots types output-management)
  #:use-module (wlroots render allocator)
  #:use-module (system repl server)
  #:use-module ((system foreign ) #:select (make-pointer scm->pointer))
  #:use-module (xkbcommon xkbcommon)
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
  #:use-module (wlroots backend libinput)
  #:use-module (wlroots backend wayland)
  #:use-module (wlroots backend x11)
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
  #:use-module (wlroots time)
  #:use-module (gwwm configuration)
  #:use-module (gwwm config)
  #:use-module (gwwm hooks)
  #:use-module (gwwm commands)
  #:use-module ((bytestructure-class) #:select (bs:enum->integer))
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (main keymap-global-set))

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
  (keymap-global-set (kbd (s e)) (lambda () (spawn "emacs")))
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
(define-public fullscreen-layer #f)
(define-public float-layer #f)
(define-public top-layer #f)
(define-public overlay-layer #f)
(define-public no-focus-layer #f)

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
      (when (and c lift)
        (wlr-scene-node-raise-to-top (client-scene c)))
      (let ((old (~ (gwwm-seat) 'keyboard-state 'focused-surface)))
        (unless (and c (equal? (client-surface c) old))
          ;; is difference
          (when (is-a? c <gwwm-client>)
            (set! (current-monitor) (client-monitor c))
            (set! (client-urgent? c) 0)
            (client-restack-surface c))

          (when (and old (or (not c) (not (equal? (client-surface c) old))))
            (if (wlr-surface-is-layer-surface old)
                (let ((l (wlr-layer-surface-v1-from-wlr-surface old)))
                  (if (and (.mapped l)
                           (member (~ l 'current 'layer) '(2 3)))
                      (return)))
                (client-activate-surface old #f)))
          (if c
              (begin (surface-notify-enter (client-surface c)
                                           (wlr-seat-get-keyboard (gwwm-seat)))
                     (client-activate-surface (client-surface c) #t ))
              (wlr-seat-keyboard-notify-clear-focus (gwwm-seat))))))))

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
                  (list-ref (slot-ref m 'tagset)
                            (slot-ref m 'seltags))
                  newtag))
        (arrange m))
      (focusclient (focustop (current-monitor)) #t))))

(define (arrange-interactive-layer m)
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
  (for-each (cut arrange-layer-client <> m box exclusive?)
            (car q-list)))
(define (arrangelayers m)
  (let* ((l (reverse (slot-ref m 'layers)))
         (box (shallow-clone (monitor-area m))))
    (for-each (cut arrangelayer m <> box #t) l)
    (unless (equal? box (monitor-window-area m))
      (set! (monitor-window-area m) (shallow-clone box))
      (arrange m))
    (for-each (cut arrangelayer m <> box #f) l)
    (arrange-interactive-layer m)))

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
  (let* ((kb (make <gwwm-keyboard> #:device device))
         (context (xkb-context-new XKB_CONTEXT_NO_FLAGS))
         (xkb-rule-names (config-xkb-rules (gwwm-config)))
         (keymap (xkb-keymap-new-from-names
                  context
                  xkb-rule-names
                  XKB_KEYMAP_COMPILE_NO_FLAGS))
         )
    (wlr-keyboard-set-keymap (.device device) keymap)
    (xkb-keymap-unref keymap)
    (xkb-context-unref context)
    (wlr-keyboard-set-repeat-info
     (.device device)
     (config-repeat-rate (gwwm-config))
     600)

    (run-hook create-keyboard-hook kb)
    ((@@ (gwwm keyboard) add-keyboard) kb)
    (add-listen (.device device) 'modifiers
                (lambda (listener data)
                  (let ((wlr-keyboard (wrap-wlr-keyboard data)))
                    (wlr-seat-set-keyboard
                     (gwwm-seat) device)
                    (run-hook modifiers-event-hook kb)
                    (wlr-seat-keyboard-notify-modifiers
                     (gwwm-seat) (.modifiers wlr-keyboard))))
                #:destroy-when device)
    (add-listen (.device device) 'key
                (lambda (listener data)
                  (let* ((event (wrap-wlr-event-keyboard-key data))
                         (seat (gwwm-seat))
                         (keybinding (@@ (gwwm keybind) keybinding)))
                    (run-hook keypress-event-hook kb event)
                    (unless (and (not (.active-inhibitor
                                       (gwwm-input-inhibit-manager)))
                                 (keybinding
                                  (wlr-keyboard-get-modifiers (.device device))
                                  (+ 8 (.keycode event))
                                  (eq? (.state event) 'WL_KEYBOARD_KEY_STATE_PRESSED)
                                  ))
                      (wlr-seat-set-keyboard seat device)
                      (wlr-seat-keyboard-notify-key
                       seat
                       (.time-msec event)
                       (.keycode event)
                       (if (eq? (.state event) 'WL_KEYBOARD_KEY_STATE_PRESSED)
                           1 0)))))
                #:destroy-when device)
    (add-listen device 'destroy
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
      ((normal) (%motionnotify time)))))

(define (idle-activity . _) (wlr-idle-notify-activity (gwwm-idle) (gwwm-seat)))

(define (cursor-setup)
  (define ((cursor/button cursor) listener data)
    (let* ((event (wrap-wlr-event-pointer-button data))
           (pressed (eq? (.state event) 'WLR_BUTTON_PRESSED)))
      (idle-activity)
      (run-hook cursor-button-event-hook event)
      (case (cursor-mode)
        ((normal) (and=> (client-at cursor)
                         (lambda (c)
                           (unless (client-is-unmanaged? c)
                             (focusclient c #t))))
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
                                (c (grabc))
                                ((not (client-is-unmanaged? c))))
                       (client-set-resizing! c #f))
                     (wlr-xcursor-manager-set-cursor-image
                      (gwwm-xcursor-manager)
                      (config-cursor-normal-image (gwwm-config))
                      cursor)
                     (cursor-mode 'normal)
                     (set! (current-monitor)
                           (monitor-at (.x cursor) (.y cursor)))
                     (setmon (grabc) (current-monitor) 0)))))))

  (let ((cursor (gwwm-cursor (wlr-cursor-create))))
    (add-listen cursor 'axis
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
    (add-listen cursor 'frame
                (lambda (listener data)
                  (let ((cursor (wrap-wlr-cursor data)))
                    (run-hook cursor-frame-event-hook cursor)
                    (wlr-seat-pointer-notify-frame (gwwm-seat))))
                #:remove-when-destroy? #f)
    (add-listen cursor 'motion
                (lambda (listener data)
                  (let ((event (wrap-wlr-event-pointer-motion data)))
                    (wlr-cursor-move (gwwm-cursor)
                                     (.device event)
                                     (.delta-x event)
                                     (.delta-y event))
                    (motionnotify (.time-msec event))))
                #:remove-when-destroy? #f)
    (add-listen cursor 'motion-absolute
                (lambda (listener data)
                  (let ((event (wrap-wlr-event-pointer-motion-absolute data)))
                    (let-slots event (device x y time-msec)
                      (wlr-cursor-warp-absolute cursor device x y)
                      (motionnotify time-msec))))
                #:remove-when-destroy? #f)
    (add-listen cursor 'button (cursor/button cursor)
                #:remove-when-destroy? #f)))

(define (client-activate-surface surface activate?)
  (cond ((and (wlr-surface-is-xdg-surface surface)
              (wlr-xdg-surface-from-wlr-surface surface)
              (eq? (.role (wlr-xdg-surface-from-wlr-surface surface))
                   'WLR_XDG_SURFACE_ROLE_TOPLEVEL))
         (wlr-xdg-toplevel-set-activated
          (wlr-xdg-surface-from-wlr-surface surface)
          activate? ))
        ((and (wlr-surface-is-xwayland-surface surface)
              (wlr-xwayland-surface-from-wlr-surface surface))
         => (cut wlr-xwayland-surface-activate <> activate?))))

(define (seat-setup display)
  (let ((seat (gwwm-seat (wlr-seat-create (gwwm-display) "seat0"))))
    (add-listen seat 'request-set-cursor
                (lambda (listener data)
                  (let ((event (wrap-wlr-seat-pointer-request-set-cursor-event
                                data)))
                    (when (eq? (cursor-mode) 'normal)
                      (let-slots event (seat-client surface hostpot-x hostpot-y)
                        (when (equal? seat-client
                                      (~ seat 'pointer-state 'focused-client))
                          (wlr-cursor-set-surface (gwwm-cursor)
                                                  surface
                                                  hostpot-x
                                                  hostpot-y)))))))
    (add-listen seat 'request-set-selection
                (lambda (listener data)
                  (let ((event (wrap-wlr-seat-request-set-selection-event data)))
                    (run-hook selection-hook event)
                    (wlr-seat-set-selection seat (.source event)
                                            (.serial event)))))
    (add-listen seat 'request-start-drag request-start-drag)
    (add-listen seat 'request-set-primary-selection setpsel)
    (add-listen (.keyboard-state seat) 'focus-change
                (lambda (listener data)
                  (let ((event (wrap-wlr-seat-keyboard-focus-change-event data)))
                    (and=> (and=> (.old-surface event)
                                  client-from-wlr-surface)
                           (cut client-set-border-color <>
                                (config-bordercolor (g-config))))
                    (and=> (and=> (.new-surface event)
                                  client-from-wlr-surface)
                           (lambda (c)
                             (q-remove! (%fstack) c)
                             (q-push! (%fstack) c)
                             (client-set-border-color
                              c
                              (config-focuscolor (g-config)))))))
                #:destroy-when seat)
    (add-listen seat 'start-drag
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
                    (add-listen icon 'destroy
                                (lambda (listener data)
                                  (remove-hook! motion-notify-hook drag-move)
                                  (wlr-scene-node-destroy scene)
                                  (focusclient (current-client) #t)
                                  (motionnotify))
                                #:remove-when-destroy? #f))))))


(define non-zero? (negate zero?))
(define (create-pointer device)
  (and-let* (((wlr-input-device-is-libinput device))
             (libinput-device (wlr-libinput-get-device-handle device)))
    (when (non-zero? (libinput-device-config-tap-get-finger-count libinput-device))
      (libinput-device-config-tap-set-enabled
       libinput-device 'LIBINPUT_CONFIG_TAP_ENABLED)
      (libinput-device-config-tap-set-drag-enabled
       libinput-device 'LIBINPUT_CONFIG_DRAG_ENABLED)
      (libinput-device-config-tap-set-drag-lock-enabled
       libinput-device 'LIBINPUT_CONFIG_DRAG_LOCK_ENABLED))
    (when (libinput-device-config-scroll-has-natural-scroll libinput-device)
      (libinput-device-config-scroll-set-natural-scroll-enabled libinput-device 0))
    (when (libinput-device-config-dwt-is-available libinput-device)
      (libinput-device-config-dwt-set-enabled libinput-device 'LIBINPUT_CONFIG_DWT_ENABLED))
    (when (libinput-device-config-left-handed-is-available libinput-device)
      (libinput-device-config-left-handed-set libinput-device 0))
    (when (libinput-device-config-middle-emulation-is-available libinput-device)
      (libinput-device-config-middle-emulation-set-enabled
       libinput-device 'LIBINPUT_CONFIG_MIDDLE_EMULATION_DISABLED))
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
(define (backend-setup display)
  (define (add-seat-capabilitie seat o)
    (wlr-seat-set-capabilities seat
                               (logior (.capabilities seat)
                                       (bs:enum->integer
                                        %wl-seat-capability-enum o))))
  (define (backend/new-output listener data)
    (let* ((wlr-output (wrap-wlr-output data))
           (m (make <gwwm-monitor>)))
      (set! (wlr-output->monitor wlr-output) m)
      (init-output wlr-output)
      (when (and (wlr-output-init-render wlr-output (gwwm-allocator) (gwwm-renderer))
                 (begin (wlr-output-set-mode wlr-output
                                             (wlr-output-preferred-mode wlr-output))
                        (wlr-output-enable wlr-output #t)
                        (wlr-output-enable-adaptive-sync wlr-output #t)
                        #t)
                 (wlr-output-commit wlr-output))
        (set! (monitor-wlr-output m) wlr-output)
        (cond ((wlr-output-is-wl wlr-output)
               (wlr-wl-output-set-title wlr-output "gwwm/wayland"))
              ((wlr-output-is-x11 wlr-output)
               (wlr-x11-output-set-title wlr-output "gwwm/x11")))
        (q-push! (%monitors) m)
        (set! (monitor-scene-output m)
              (wlr-scene-output-create (gwwm-scene) wlr-output))
        (wlr-output-layout-add-auto (gwwm-output-layout) wlr-output)

        (set! (monitor-layouts m)
              (make-list 2 tile-layout))
        (add-listen (monitor-wlr-output m) 'frame (render-monitor m))
        (add-listen (monitor-wlr-output m) 'destroy (cleanup-monitor m))
        (run-hook create-monitor-hook m))))
  (define (backend/new-input listener data)
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
        (else (send-log WARNING "unknow input device")))))
  (let ((backend (or (and=> (wlr-backend-autocreate(gwwm-display)) gwwm-backend)
                     (begin (send-log ERROR (G_ "gwwm Couldn't create backend"))
                            (exit 1)))))
    (add-listen backend 'new-input backend/new-input)
    (add-listen backend 'new-output backend/new-output)))

(define (output-manager-setup display)
  (let ((output-manager (gwwm-output-manager (wlr-output-manager-v1-create display))))


    (add-listen output-manager 'apply
                (lambda (listener data)
                  (let ((config (wrap-wlr-output-configuration-v1 data)))
                    (output-manager-apply-or-test config #f))))
    (add-listen output-manager 'test
                (lambda (listener data)
                  (let ((config (wrap-wlr-output-configuration-v1 data)))
                    (output-manager-apply-or-test config #t))))))

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
  (gwwm-xdg-shell (wlr-xdg-shell-create (gwwm-display)))
  (add-listen (gwwm-xdg-shell) 'new-surface
              (lambda (listener data)
                (let ((xdg-surface (wrap-wlr-xdg-surface data)))
                  (when (eq? (.role xdg-surface)
                             'WLR_XDG_SURFACE_ROLE_TOPLEVEL)
                    (let ((c (make <gwwm-xdg-client>)))
                      (set! (.data xdg-surface) (scm->pointer c))
                      (set! (client-super-surface c) xdg-surface)
                      (set! (client-border-width c) (gwwm-borderpx))
                      (run-hook create-client-hook c))))))
  (gwwm-compositor (wlr-compositor-create (gwwm-display) (gwwm-renderer)))
  (gwwm-activation (wlr-xdg-activation-v1-create (gwwm-display)))
  (add-listen (gwwm-activation) 'request-activate
              (lambda (listener data)
                (let* ((event (wrap-wlr-xdg-activation-v1-request-activate-event data))
                       (c (client-from-wlr-surface (.surface event))))
                  (unless (equal? c (current-client))
                    (set! (client-urgent? c) #t)))))
  (gwwm-layer-shell (wlr-layer-shell-v1-create (gwwm-display)))
  (add-listen (gwwm-layer-shell) 'new-surface
              (lambda (listener data)
                (let* ((layer-surface (wrap-wlr-layer-surface-v1 data))
                       (c (make <gwwm-layer-client>)))
                  (unless (.output layer-surface)
                    (set! (.output layer-surface)
                          (monitor-wlr-output (current-monitor))))
                  (pk  'layer (~ layer-surface 'pending 'layer))
                  (let ((node (wlr-scene-subsurface-tree-create
                               (return-scene-node (~ layer-surface 'pending 'layer))
                               (.surface layer-surface))))
                    (set! (client-scene c) node)
                    (set! (.data (.surface layer-surface)) (unwrap-wlr-scene-node node)))
                  (set! (client-super-surface c) layer-surface)
                  (set! (client-monitor c) (wlr-output->monitor (.output layer-surface)))
                  (set! (.data (client-scene c)) (scm->pointer c))
                  (set! (.data layer-surface) (scm->pointer c))
                  (q-push! (list-ref (slot-ref (client-monitor c) 'layers)
                                     (~ layer-surface 'pending 'layer))
                           c)
                  ;; Temporarily set the layer's current state to pending
                  ;; so that we can easily arrange it
                  (let ((old-state (~ layer-surface 'current))
                        (new-state (~ layer-surface 'pending)))
                    (set! (.current layer-surface) new-state)
                    (arrangelayers (wlr-output->monitor (.output layer-surface)))
                    (set! (.current layer-surface) old-state))

                  (run-hook create-client-hook c))))
  (gwwm-idle (wlr-idle-create (gwwm-display)))
  (gwwm-output-layout (wlr-output-layout-create))
  (add-listen (gwwm-output-layout) 'change update-monitors)
  (add-hook! keypress-event-hook idle-activity)
  (output-manager-setup (gwwm-display))
  (wlr-cursor-attach-output-layout (gwwm-cursor) (gwwm-output-layout)))

(define (xwayland-setup display compositor)
  (let ((x (gwwm-xwayland (wlr-xwayland-create display compositor #t))))
    (if x
        (begin
          (wl-signal-add (get-event-signal x 'ready)
                         xwaylandready)
          (add-listen x 'new-surface
                      (lambda (listener data)
                        (for-each (lambda (c)
                                    (when (and (client-fullscreen? c)
                                               (visibleon c (client-monitor c)))
                                      (client-do-set-fullscreen c #f)))
                                  (client-list))
                        (let* ((xsurface (wrap-wlr-xwayland-surface data))
                               (c (make <gwwm-x-client>)))
                          (set! (.data xsurface) (scm->pointer c))
                          (set! (client-border-width c) (gwwm-borderpx))
                          (set! (client-super-surface c) xsurface)
                          (run-hook create-client-hook c)))
                      #:remove-when-destroy? #f)
          (setenv "DISPLAY" (wlr-xwayland-display-name x)))
        (send-log INFO (G_ "failed to setup XWayland X server, continuing without it.")))))

(define ((unmap-notify* c) listener data)
  (when (equal? c (grabc))
    (cursor-mode 'normal)
    (grabc #f))
  (and=> (client-monitor c)
         (cut slot-set! <> 'un-map #t))
  (when (client-is-unmanaged? c)
    (wlr-scene-node-destroy (client-scene c)))

  (unless (client-is-unmanaged? c)
    (setmon c #f 0)
    (q-remove! (%clients) c)
    (q-remove! (%fstack) c)
    (wlr-scene-node-destroy (client-scene c))
    (set! (client-scene c) #f)))

(define (apply-rules c)
  (client-set-floating! c (client-is-float-type? c))
  (%applyrules c))

(define (map-notify* c)
  (lambda (listener data)
    (run-hook client-map-event-hook c
              ((if (client-is-x11? c)
                   wrap-wlr-xwayland-surface
                   wrap-wlr-xdg-surface)
               data))
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
    (add-listen (client-surface c) 'commit
                (lambda (a b)
                  (let ((client c))
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
    (let ((p (scm->pointer c)))
      (set! (.data (client-scene c)) p)
      (set! (.data (client-scene-surface c)) p))
    (if (client-is-unmanaged? c)
        (begin (wlr-scene-node-reparent (client-scene c) float-layer)
               (wlr-scene-node-set-position
                (client-scene c)
                (+ (box-x (client-geom c)) (gwwm-borderpx))
                (+ (box-y (client-geom c)) (gwwm-borderpx))))
        (begin (client-init-border c)
               (q-push! (%clients) c)
               (q-push! (%fstack) c)

               (let ((parent (client-get-parent c)))
                 (if parent
                     (begin (setmon c (or (client-monitor parent)
                                          (current-monitor))
                                    (client-tags parent))
                            (client-do-set-floating c #t))
                     (apply-rules c)))
               (client-do-set-fullscreen c )
               (slot-set! (client-monitor c) 'un-map #f)))))

(define ((map-layer-client-notify c) listener data)
  (wlr-surface-send-enter
   (client-surface c)
   (monitor-wlr-output(client-monitor c)))
  (motionnotify))
(define ((unmap-layer-client-notify c) listener data)
  (wlr-scene-node-set-enabled (client-scene c) #f)
  (when (equal? (client-surface c) (exclusive-focus))
    (exclusive-focus #f))
  (when (equal? (client-surface c)
                (.focused-surface
                 (.keyboard-state (gwwm-seat))))
    (focusclient (current-client) #f))
  (motionnotify))

(define ((render-monitor m) listener data)
  (let ((_ now (clock-gettime 1))
        (skip? #f))
    (for-each
     (lambda (c)
       (and-let* ((serial (client-resize-configure-serial c))
                  ((is-a? c <gwwm-xdg-client>))
                  (current (.current
                            (client-super-surface c)))
                  (pending (.pending
                            (client-super-surface c))))
         (when (or (and (not (zero? serial))
                        (slot-ref m 'un-map))
                   (not (= (~ pending 'geometry 'width)
                           (~ current 'geometry 'width)))
                   (not (= (~ pending 'geometry 'height)
                           (~ current 'geometry 'height))))
           (wlr-surface-send-frame-done (client-surface c) now)
           (set! skip? #t))))

     (client-list))

    (if (and (not skip?) (not (wlr-scene-output-commit (monitor-scene-output m))))
        #f
        (begin (wlr-scene-output-send-frame-done (monitor-scene-output m) now)
               (slot-set! m 'un-map #f)))))

(define ((cleanup-monitor m) listener data)
  (q-remove! (%monitors) m)
  (wlr-output-layout-remove
   (gwwm-output-layout)
   (monitor-wlr-output m))

  (wlr-scene-output-destroy (monitor-scene-output m))
  (set! (monitor-scene-output m) #f)
  (let* ((ms (monitor-list))
         (l (length ms)))
    (unless (zero? l)
      (and=> (find (lambda (m)
                     (.enabled (monitor-wlr-output m))) ms)
             current-monitor)))

  (focusclient (focustop (current-monitor)) #t)
  (closemon m))

(define (scene-setup display backend)
  (let ((scene (gwwm-scene (wlr-scene-create))))
    (wlr-scene-set-presentation scene (wlr-presentation-create display backend))
    (let ((create (lambda () (.node (wlr-scene-tree-create (.node scene))))))
      (set! background-layer (create))
      (set! bottom-layer (create))
      (set! tile-layer (create))
      (set! fullscreen-layer (create))
      (set! float-layer (create))
      (set! top-layer (create))
      (set! overlay-layer (create))
      (set! no-focus-layer (create)))))

(define (main)
  (setlocale LC_ALL "")
  (textdomain %gettext-domain)
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
  (define ((new-popup-notify c) listener data)
    (let* ((popup (wrap-wlr-xdg-popup data))
           (node (wlr-scene-xdg-surface-create
                  (wrap-wlr-scene-node (~ popup 'parent 'data))
                  (.base popup))))
      (add-listen (.base popup) 'new-popup (new-popup-notify c))
      (run-hook create-popup-hook popup)

      (set! (.data (.surface (.base popup))) (get-pointer node))

      (and-let* (c
                 (monitor (client-monitor c))
                 (geom (shallow-clone
                        (if (is-a? c <gwwm-layer-client>)
                            (monitor-area monitor)
                            (monitor-window-area (client-monitor c))))))
        (unless (or (is-a? c <gwwm-layer-client>) (client-floating? c))
          (wlr-scene-node-raise-to-top (.parent node)))
        (modify-instance* geom
          (x (- x (box-x (client-geom c))))
          (y (- y (box-y (client-geom c)))))
        (wlr-xdg-popup-unconstrain-from-box popup geom))))

  (define ((set-title-notify c) listener data)
    (let ((title (client-title c))
          (new (client-get-title c)))
      (set! (client-title c) new )
      (run-hook update-title-hook c title new)))

  (define ((request-fullscreen-notify c) listener data)
    (let ((fullscreen? (client-wants-fullscreen? c))
          (event-or-xsurface ((if (client-is-x11? c)
                                  wrap-wlr-xwayland-surface
                                  wrap-wlr-xdg-toplevel-set-fullscreen-event)

                              data)))
      (if (client-monitor c)
          (client-do-set-fullscreen c fullscreen?)
          (set! (client-fullscreen? c) fullscreen?))
      (run-hook fullscreen-event-hook c event-or-xsurface)))
  (add-hook!
   create-client-hook
   (lambda (c)
     (when (is-a? c <gwwm-client>)
       (set! (client-appid c) (client-get-appid c))
       (set! (client-title c) (client-get-title c))
       (add-listen (client-super-surface c) 'unmap (unmap-notify* c))
       (add-listen (client-super-surface c) 'map (map-notify* c)))
     (cond ((is-a? c <gwwm-xdg-client>)
            (add-listen (client-super-surface c) 'new-popup
                        (new-popup-notify c))
            (add-listen (wlr-xdg-surface-toplevel (client-super-surface c))
                        'set-title (set-title-notify c)
                        #:destroy-when (client-super-surface c))

            (add-listen (wlr-xdg-surface-toplevel (client-super-surface c))
                        'set-app-id
                        (lambda (listener data)
                          (set! (client-appid c)
                                (client-get-appid c)))
                        #:destroy-when (client-super-surface c))
            (add-listen (wlr-xdg-surface-toplevel (client-super-surface c))
                        'request-fullscreen
                        (request-fullscreen-notify c)
                        #:destroy-when (client-surface c)))
           ((is-a? c <gwwm-x-client>)
            (add-listen (client-super-surface c) 'set-class
                        (lambda (listener data)
                          (set! (client-appid c)
                                (client-get-appid c)))
                        #:destroy-when (client-super-surface c))
            (add-listen (client-super-surface c) 'request-activate
                        (lambda (listener data)
                          (let ((xsurface (wrap-wlr-xwayland-surface data)))
                            (when (and (.mapped xsurface)
                                       (not (client-is-unmanaged? c)))
                              (wlr-xwayland-surface-activate xsurface #t)))))
            (add-listen (client-super-surface c) 'request-configure
                        (lambda (listener data)
                          (let ((event (wrap-wlr-xwayland-surface-configure-event data)))
                            (let-slots event (surface x y width height)
                              (wlr-xwayland-surface-configure surface x y width height)))))
            (add-listen (client-super-surface c) 'set-hints
                        (lambda (listener data)
                          (let ((xsurface (wrap-wlr-xwayland-surface data)))
                            (when (and (.mapped xsurface)
                                       (.surface xsurface))
                              (set! (client-urgent? c)
                                    (.hints-urgency xsurface))))))
            (add-listen (client-super-surface c) 'request-fullscreen
                        (request-fullscreen-notify c))
            (add-listen (client-super-surface c) 'set-title
                        (set-title-notify c)))
           ((is-a? c <gwwm-layer-client>)
            (q-push! (%layer-clients) c)
            (add-listen (client-super-surface c) 'map
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
                               (client-scene c)
                               (return-scene-node
                                (~ layer-surface 'current 'layer)))
                              (q-push! (list-ref
                                        (slot-ref m 'layers)
                                        (~ layer-surface 'current 'layer))
                                       c))
                            ;; (commit-layer-client-notify c listener data)
                            (unless (zero?
                                     (~ layer-surface
                                        'current
                                        'committed))
                              (arrangelayers m)))))
            (add-listen (client-super-surface c) 'destroy
                        (lambda (listener data)
                          (q-remove! (%layer-clients) c)
                          (for-each (cut q-remove! <> c)
                                    (slot-ref (client-monitor c) 'layers))
                          (and=> (client-monitor c) arrangelayers))
                        #:remove-when-destroy? #f)
            (add-listen (client-super-surface c) 'unmap
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
  (scene-setup (gwwm-display) (gwwm-backend))
  (%gwwm-setup-signal)
  (%gwwm-setup-othres)
  (gwwm-input-inhibit-manager (wlr-input-inhibit-manager-create (gwwm-display)))
  (%gwwm-setup)
  (config-setup)
  (when (config-enable-xwayland? (gwwm-config))
    (xwayland-setup (gwwm-display) (gwwm-compositor)))
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
