(add-to-load-path (dirname(dirname (current-filename))))
(define-module (gwwm init)
  #:use-module (gwwm util)
  #:use-module (srfi srfi-26)
  #:use-module (system repl server)
  ;;  #:use-module (oop goops)
  #:use-module (wayland)
  #:use-module (ice-9 getopt-long)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (wlroots backend)
  #:export (handle-keybinding
            shutdown-hook
            gwwm-wl-display
            gwwm-server-backend
            gwwm-init-socket
            gwwm-run!))
;;(define-values (program-name arguments) (car+cdr (program-arguments)))
(define startup-cmd "")
(let* ((option-spec
        '((version (single-char #\v) (value #f))
          (help (single-char #\h) (value #f))
          (exec (single-char #\s) (value #t))))
       (options (getopt-long (command-line) option-spec))
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
             (exit 0))
      (and=> (option-ref options 'exec #f)
             (cut set! startup-cmd <>))))
;; (define-class <server> ()
;;   display)

;;(define server )

(define gwwm-wl-display (wl-display-create))
(wl-display-init-shm gwwm-wl-display)
(define gwwm-server-backend (wlr-backend-autocreate gwwm-wl-display))
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
  (unless (string-null? startup-cmd)
    (fork+exec startup-cmd))
  (wl-display-run gwwm-wl-display)
  (run-hook shutdown-hook)
  (wl-display-destroy-clients gwwm-wl-display)
  (wl-display-destroy gwwm-wl-display))

(define (handle-keybinding s key)
  ;;(run-hook )
  (case key
    ((100)
     (fork+exec "emacs"))
    ((101)
     (fork+exec "alacritty"))
    ((#xff1b)
     (wl-display-terminate gwwm-wl-display)
     #t)
    (else #f)))

(define keyboard-pass-hook (make-hook 2))
(define shutdown-hook (make-hook))
(add-hook! shutdown-hook (lambda _ (display "shutdown-now!") (newline)))

(false-if-exception(spawn-server (make-tcp-server-socket)))
