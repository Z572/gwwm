(add-to-load-path (dirname(dirname (current-filename))))
(define-module (gwwm init)
  #:use-module (system repl server)
  #:use-module (wayland)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1))
(define-values (program-name arguments) (car+cdr (program-arguments)))
;; (define startup-cmd %null-pointer)
;; (case  (first arguments)
;;   (("-l")
;;    (primitive-load (second arguments)))
;;   (("-s")
;;    (set! startup-cmd (second arguments))))

(define keyboard-pass-hook (make-hook 2))
(add-hook! keyboard-pass-hook
           (lambda a
             (pk a)))
(add-hook! keyboard-pass-hook
           (lambda (a b)
             (if (and (= b 100)
                      (= (primitive-fork) 0))
                 (execlp "alacritty"))))
(define shutdown-hook (make-hook 1))
(add-hook! shutdown-hook (lambda (a) (pk 'shutdown-now!)))

(spawn-server (make-tcp-server-socket))
