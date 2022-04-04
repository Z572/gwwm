(add-to-load-path (dirname(dirname (current-filename))))
(define-module (gwwm init)
  #:use-module (gwwm util)
  #:use-module (system repl server)
  ;;  #:use-module (oop goops)
  #:use-module (wayland)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:export (handle-keybinding
            shutdown-hook
            gwwm-wl-display
            gwwm-init-socket))
(define-values (program-name arguments) (car+cdr (program-arguments)))
;; (define-class <server> ()
;;   display)

;;(define server )
(define gwwm-wl-display (wl-display-create))
(wl-display-init-shm gwwm-wl-display)
(define (gwwm-init-socket)
  (and=> (wl-display-add-socket-auto gwwm-wl-display)
         (lambda (a)
           (setenv "WAYLAND_DISPLAY" a)
           a)))

(define (handle-keybinding s key)
  ;;(run-hook )
  (case key
    ((100)
     (fork+exec "emacs"))
    ((101)
     (fork+exec "alacritty"))
    ((#xff1b)
     (wl-display-terminate (wrap-wl-display s))
     #t)
    (else #f)))

(define keyboard-pass-hook (make-hook 2))
(define shutdown-hook (make-hook 1))
(add-hook! shutdown-hook (lambda (a) (display "shutdown-now!") (newline)))

(false-if-exception(spawn-server (make-tcp-server-socket)))
