(add-to-load-path (dirname(dirname (current-filename))))
(define-module (gwwm init)
  #:use-module (gwwm util)
  #:use-module (srfi srfi-26)
  #:use-module (system repl server)
  ;;  #:use-module (oop goops)
  #:use-module (wayland)
  #:use-module (wayland util)
  #:use-module (ice-9 getopt-long)
  ;;  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (wlroots backend)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots render allocator)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types data-device)
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types scene)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots types cursor)
  #:use-module (wlroots types xcursor)
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
(let* (;; (option-spec
       ;;  '((version (single-char #\v) (value #f))
       ;;    (help (single-char #\h) (value #f))
       ;;    (exec (single-char #\s) (value #t))))
       ;; (options (getopt-long (command-line) option-spec))
       (help-wanted (option-ref options 'help #f))
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
(define startup-cmd (option-ref options 'exec #f))
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

     (seat ,(bs:pointer '*))
     (new-input ,%wl-listener)
     (request-cursor ,%wl-listener)
     (request-set-selection ,%wl-listener)
     (keyboards ,%wl-listener)
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

(define pointer->server-bytestructure
  (cut pointer->bytestructure <> %server-struct))
(define gwwm-wl-display (wl-display-create))
(wl-display-init-shm gwwm-wl-display)
(pk 'a)
(define-public (server-new-xdg-surface s data)
  (pk 'server-new-xdg-surface s data
      (pointer->server-bytestructure s)

      (bytestructure-ref (pointer->bytestructure data %wlr-xdg-surface-struct) 'role)
      ;;(bytestructure-ref (pointer->bytestructure data %wlr-xdg-surface-struct) 'added)
      ;; (bytestructure-ref (pointer->bytestructure data %wlr-xdg-shell-struct) )
      ))
(define gwwm-server-backend (pk 'a (wlr-backend-autocreate gwwm-wl-display)))
;; (pk gwwm-server-backend (pk 'signal-added
;;                             (wl-signal-add
;;                              (pk 'b(get-event-signal gwwm-server-backend 'new-input))
;;                              (pk 'c (make-wl-listener
;;                                      (pk 'd(let ((a (wl-list-init (pk 's(make-wl-list)))))
;;                                              (pk (wl-list-init (pk 'aa a)))
;;                                              a))
;;                                      (lambda a (pk 'nnn a)))))
;;                             )
;;     )
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

(define (gwwm-run!)
  (pk 'run!)
  (when startup-cmd
    (fork+exec startup-cmd))

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
