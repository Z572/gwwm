(use-modules (ice-9 getopt-long)
             (ice-9 format)
             (system repl server)
             (gwwm keymap)
             (gwwm monitor)
             (gwwm utils srfi-215)
             (wlroots types output)
             (wlroots types seat)
             (gwwm hooks)
             (gwwm commands)
             (srfi srfi-1)
             (gwwm config))
(setlocale LC_ALL "")
(define option-spec
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (exec (single-char #\s) (value #t))))
(define-public (parse-command-line)
  (let* ((options (getopt-long (command-line) option-spec))
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
  -s --exec     run program
"))
               (exit 0)))))



(current-log-callback
 (lambda (msg)
   (let ((p (current-error-port))
         (msg2 msg))
     (format p "[~a]| ~a | "
             (cdr (assq 'SEVERITY msg))
             (cdr (assq 'MESSAGE msg)))
     (set! msg2 (assoc-remove! (assoc-remove! msg2 'SEVERITY) 'MESSAGE))
     (for-each (lambda (a)
                 (display (car a) p)
                 (display ":" p)
                 (display (object->string(cdr a)) p)
                 (display " " p))
               msg2)
     (newline p))))


(false-if-exception (spawn-server (make-tcp-server-socket)))

(define-once global-keymap
  (make-parameter (make-keymap)))

(define (keymap-global-set key command)
  (keymap-set (global-keymap) key command))

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

(define (set-mode m)
  (let ((output (monitor-wlr-output m)))
    (wlr-output-set-mode output (wlr-output-preferred-mode output))))
(add-hook! create-monitor-hook set-mode)

(define (pass-modifiers k)
  (wlr-seat-set-keyboard (gwwm-seat) (keyboard-input-device k)))
(add-hook! modifiers-event-hook pass-modifiers )
