(add-to-load-path (dirname(dirname (current-filename))))
(define-module (gwwm init)
  #:use-module (gwwm util)
  #:use-module (system repl server)
  #:use-module (wayland)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:export (handle-keybinding
            shutdown-hook
            gwwm-wl-display))
(define-values (program-name arguments) (car+cdr (program-arguments)))
(define gwwm-wl-display (wl-display-create))


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
(setenv "GWWM" "1")


(define keyboard-pass-hook (make-hook 2))
(define shutdown-hook (make-hook 1))
(add-hook! shutdown-hook (lambda (a) (display "shutdown-now!") (newline)))

(spawn-server (make-tcp-server-socket))
