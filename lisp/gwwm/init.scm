(add-to-load-path (dirname(dirname (current-filename))))
(define-module (gwwm init)
  #:use-module (gwwm util)
  #:use-module (srfi srfi-26)
  #:use-module (system repl server)
  ;;  #:use-module (oop goops)
  #:use-module (wayland)
  #:use-module (wayland util)
  #:use-module (ice-9 getopt-long)
  #:use-module ((system foreign) #:select (make-pointer pointer-address void procedure->pointer))
  #:use-module (srfi srfi-1)
  #:use-module (wlroots util log)
  #:use-module (wlroots backend)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots render allocator)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots types data-device)
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types scene)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots types layer-shell)
  #:use-module (wlroots types output)
  #:use-module (wlroots types seat)
  #:use-module (wlroots types cursor)
  #:use-module (wlroots types xcursor)
  #:use-module (wlroots types pointer)
  #:use-module (bytestructures guile)
  #:duplicates (merge-generics)
  #:export (handle-keybinding
            shutdown-hook
            gwwm-wl-display
            gwwm-server-backend
            gwwm-init-socket
            gwwm-run!))
;;(define-values (program-name arguments) (car+cdr (program-arguments)))

(define option-spec
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (exec (single-char #\s) (value #t))))
(define options (getopt-long (command-line) option-spec))
(let* ((help-wanted (option-ref options 'help #f))
       (version-wanted (option-ref options 'version #f)))
  (if (or version-wanted help-wanted)
      (begin (if version-wanted
                 (display "gwwm v0.0.1
"))
             (if help-wanted
                 (display "\
gwwm [options]
  -v --version  Display version
  -h --help     Display this help
"))
             (exit 0))))

;; (define-class <server> ()
;;   display)

;;(define server )
(define %server-struct
  (bs:struct
   `((scene ,(bs:pointer '*))
     (xdg-shell ,(bs:pointer '*))
     (new-xdg-surface ,%wl-listener)
     (views ,%wl-list)
     (cursor ,(bs:pointer '*))
     (xcursor-manager ,(bs:pointer '*))
     (cursor-motion ,%wl-listener)
     (cursor-motion-absolute ,%wl-listener)
     (cursor-button ,%wl-listener)
     (cursor-axis ,%wl-listener)
     (cursor-frame ,%wl-listener)

     (seat ,(bs:pointer %wlr-seat-struct))
     (new-input ,%wl-listener)
     (request-cursor ,%wl-listener)
     (request-set-selection ,%wl-listener)
     (keyboards ,%wl-list)
     (grabbed-view ,(bs:pointer '*))
     (grab-x ,double)
     (grab-y ,double)
     (grab-geobox ,(bs:pointer '*))
     (resize-edges ,uint32)
     (output-layout ,(bs:pointer '*))
     (outputs ,%wl-list)
     (new-output ,%wl-listener)

                                        ;(cursor-module ,)
     )))
(define %tinywl-view-struct
  (bs:struct `((link ,%wl-list)
               (server ,(bs:pointer %server-struct))
               (xdg-surface ,(bs:pointer %wlr-xdg-surface-struct))
               (scene-node ,(bs:pointer '*))
               (map ,%wl-listener)
               (unmap ,%wl-listener)
               (destroy ,%wl-listener)
               (request-move ,%wl-listener)
               (request-resize ,%wl-listener)
               (x ,int)
               (y ,int))))

(define pointer->server-bytestructure
  (cut pointer->bytestructure <> %server-struct))
(wlr-log-init 'debug)
(define gwwm-wl-display (wl-display-create))
(wl-display-init-shm gwwm-wl-display)
(define-public (server-new-xdg-surface s data)
  (pk 'server-new-xdg-surface s data
      (pointer->server-bytestructure s)

      (bytestructure-ref (pointer->bytestructure data %wlr-xdg-surface-struct) 'role)
      ;;(bytestructure-ref (pointer->bytestructure data %wlr-xdg-surface-struct) 'added)
      ;; (bytestructure-ref (pointer->bytestructure data %wlr-xdg-shell-struct) )
      ))
(define gwwm-server-backend (pk 'a (wlr-backend-autocreate gwwm-wl-display)))

(define-public gwwm-server-renderer (wlr-renderer-autocreate gwwm-server-backend))
(pk 'c)
(define-public gwwm-server-allocator (wlr-allocator-autocreate gwwm-server-backend gwwm-server-renderer))
(define-public gwwm-server-compositor (wlr-compositor-create gwwm-wl-display gwwm-server-renderer))
(define-public gwwm-server-data-device-manager (wlr-data-device-manager-create gwwm-wl-display))
(define-public gwwm-server-scene (wlr-scene-create))

(wlr-renderer-init-wl-display gwwm-server-renderer gwwm-wl-display)

(define-public gwwm-server-output-layout (wlr-output-layout-create))
(wlr-scene-attach-output-layout gwwm-server-scene gwwm-server-output-layout)
(define-public gwwm-server-xdg-shell (wlr-xdg-shell-create gwwm-wl-display))
(define-public gwwm-server-cursor (wlr-cursor-create))
(wlr-cursor-attach-output-layout gwwm-server-cursor gwwm-server-output-layout)
(define-public gwwm-server-cursor-mgr (wlr-xcursor-manager-create #f 12))
(wlr-xcursor-manager-load gwwm-server-cursor-mgr 1)

(define-public gwwm-server-seat (wlr-seat-create gwwm-wl-display "seat0"))
(define-public gwwm-server-layer-shell (wlr-layer-shell-v1-create gwwm-wl-display))
(define-public server-cursor-mode 0)
(define-public (set-server-cursor-mode a) (pk 'set! 'server-cursor-mode server-cursor-mode 'to a)(set! server-cursor-mode a))
(define (gwwm-init-socket)
  (let ((socket (wl-display-add-socket-auto gwwm-wl-display)))
    (if socket
        (begin
          (setenv "WAYLAND_DISPLAY" socket)
          (when (= (wlr-backend-start gwwm-server-backend) 0)
            (wlr-backend-destroy gwwm-server-backend)
            (wl-display-destroy gwwm-wl-display)))
        (wlr-backend-destroy gwwm-server-backend)))
  )

(define-public (wlr_xcursor_manager_set_cursor_image name)
  (wlr-xcursor-manager-set-cursor-image gwwm-server-cursor-mgr name gwwm-server-cursor))

(define-public (disable-toplevel-activated surface)
  (wlr-xdg-toplevel-set-activated
   (wlr-xdg-surface-from-wlr-surface surface) #f))
(define (server-cursor-frame listener data)
  (let ((server (wl-container-of listener %server-struct 'cursor-frame)))
    (wlr-seat-pointer-notify-frame gwwm-server-seat)))
(define-public server-cursor-frame-pointer
  (procedure->pointer void server-cursor-frame '(* *)))

(define-public (xdg-toplevel-destroy listener data)
  (let* ((view (pk 'view (wl-container-of listener %tinywl-view-struct 'destroy))))
    (pk 'destroy)
    ;; FIXME: why need use @ ? just a for-each will not found for-each variable
    ((@ (guile) for-each)
     (lambda (a) (let ((l (wl-list-init (.link (wrap-wl-listener (bytestructure-ref view a ))))))
                   (pk 'c (wl-list-length l))
                   (pk 'a (wl-list-remove l))
                                        ;(pk 'b(wl-list-length l))
                   ))
     '(map unmap destroy request-move request-resize))))
(define-public xdg-toplevel-destroy-pointer
  (procedure->pointer void xdg-toplevel-destroy '(* *)))

(define (gwwm-run!)
  (pk 'run!)
  (and=> (option-ref options 'exec #f) fork+exec)

  (wl-display-run gwwm-wl-display)
  (run-hook shutdown-hook)
  (wl-display-destroy-clients gwwm-wl-display)
  (wl-display-destroy gwwm-wl-display))

(define (handle-keybinding server modifiers key)
  (pk (bytestructure-ref (pointer->bytestructure server %server-struct) 'grab-x) 'key-is modifiers key)

  ;;(run-hook )
  (and (= modifiers 64)
       (case key
         ((100)
          (fork+exec "emacs"))
         ((101)
          (fork+exec "alacritty"))
         ((#xff1b)
          (wl-display-terminate gwwm-wl-display)
          #t)
         (else #f))))


(define keyboard-pass-hook (make-hook 2))
(define shutdown-hook (make-hook))
(add-hook! shutdown-hook (lambda _ (display "shutdown-now!") (newline)))

(false-if-exception(spawn-server (make-tcp-server-socket)))

(define-public (server-new-output listener data)
  (let ((server (wl-container-of listener %server-struct 'new-output))
        (output (wrap-wlr-output data)))
    (wlr-output-init-render output
                            gwwm-server-allocator
                            gwwm-server-renderer)
    (if (not (wl-list-empty (.modes output)))
        (let ((mode (wlr-output-preferred-mode output)))
          (wlr-output-set-mode output mode)
          (wlr-output-enable output #t)))
    (wlr-output-commit output)))

(define-public (gwwm-seat-request-cursor p1 p2 )
  (let* ((server-bytestructure (wl-container-of p1 %server-struct 'request-cursor))
         (event (pointer->bytestructure p2 %wlr-seat-request-set-cursor-event-struct))
         (focused-client (wrap-wlr-seat-client
                          (make-pointer
                           (bytestructure-ref
                            server-bytestructure 'seat 'pointer-state 'focused-client))))
         (seat-client (wrap-wlr-seat-client (make-pointer (bytestructure-ref event 'seat-client)))))
    (if (= focused-client seat-client)
        (wlr-cursor-set-surface (wrap-wlr-cursor
                                 (make-pointer (bytestructure-ref server-bytestructure 'cursor)))
                                (wrap-wlr-surface
                                 (make-pointer (bytestructure-ref event 'surface)))
                                (bytestructure-ref event 'hostpot-x)
                                (bytestructure-ref event 'hostpot-y)))))
(define-public gwwm-seat-request-cursor-pointer
  (procedure->pointer void gwwm-seat-request-cursor '(* *)))
(define-public (gwwm-seat-request-set-selection p1 p2)
  (let* ((server-bytestructure (wl-container-of p1 %server-struct 'request-set-selection))
         (seat (wrap-wlr-seat
                (make-pointer
                 (bytestructure-ref
                  server-bytestructure 'seat))))
         (event (pointer->bytestructure p2 %wlr-seat-request-set-selection-event-struct))
         (wlr-data-source (wrap-wlr-data-source
                           (make-pointer (bytestructure-ref event 'source))))
         (serial (bytestructure-ref event 'serial)))
    (wlr-seat-set-selection seat wlr-data-source serial)))
(define-public gwwm-seat-request-set-selection-pointer
  (procedure->pointer void gwwm-seat-request-set-selection '(* *)))

(define (server-cursor-motion listener data)
  (let ((server (wl-container-of listener %server-struct 'cursor-motion))
        (event (pointer->bytestructure data %wlr-event-pointer-motion-struct)))
    (wlr-cursor-move gwwm-server-cursor
                     (wrap-wlr-input-device (make-pointer (bytestructure-ref event 'device)))
                     (bytestructure-ref event 'delta-x)
                     (bytestructure-ref event 'delta-y))
    (process-cursor-motion (bytestructure->pointer server) (bytestructure-ref event 'time-msec))))
(define-public server-cursor-motion-pointer
  (procedure->pointer void server-cursor-motion '(* *)))

(define (server-cursor-motion-absolute listener data)
  (let ((server (wl-container-of listener %server-struct 'cursor-motion-absolute))
        (event (pointer->bytestructure data %wlr-event-pointer-motion-absolute-struct)))
    (wlr-cursor-warp-absolute gwwm-server-cursor
                              (wrap-wlr-input-device (make-pointer (bytestructure-ref event 'device)))
                              (bytestructure-ref event 'x)
                              (bytestructure-ref event 'y))
    (process-cursor-motion (bytestructure->pointer server)
                           (bytestructure-ref event 'time-msec))))
(define-public server-cursor-motion-absolute-pointer
  (procedure->pointer void server-cursor-motion-absolute '(* *)))

(define (server-cursor-axis l d)
  (let ((server (wl-container-of l %server-struct 'cursor-axis))
        (event (pointer->bytestructure d %wlr-event-pointer-axis-struct)))
    (wlr-seat-pointer-notify-axis
     gwwm-server-seat
     (bytestructure-ref event 'time-msec)
     (bytestructure-ref event 'orientation)
     (bytestructure-ref event 'delta)
     (bytestructure-ref event 'delta-discrete)
     (bytestructure-ref event 'source))))
(define-public server-cursor-axis-pointer
  (procedure->pointer void server-cursor-axis '(* *)))

(define (xdg-toplevel-unmap l d)
  (let ((view (wl-container-of l %tinywl-view-struct 'unmap)))
    (wl-list-remove (wrap-wl-list (bytestructure-ref view 'link)))))
(define-public xdg-toplevel-unmap-pointer
  (procedure->pointer void xdg-toplevel-unmap '(* *)))
(define (xdg-toplevel-request-move l d)
  (let ((view (wl-container-of l %tinywl-view-struct 'request-move)))
    (begin-interactive (bytestructure->pointer view) 1 0)))
(define-public xdg-toplevel-request-move-pointer
  (procedure->pointer void xdg-toplevel-request-move '(* *)))

(define (xdg-toplevel-request-resize l d)
  (let* ((event (wrap-wlr-xdg-toplevel-resize-event d))
         (view (wl-container-of l %tinywl-view-struct 'request-resize)))
    (begin-interactive (bytestructure->pointer view) 2 (.edges event))))
(define-public xdg-toplevel-request-resize-pointer
  (procedure->pointer void xdg-toplevel-request-resize '(* *)))
(define-public (server-new-pointer server device)
  (wlr-cursor-attach-input-device
   gwwm-server-cursor
   (wrap-wlr-input-device device)))
