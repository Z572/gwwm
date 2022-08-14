(use-modules (ice-9 getopt-long)
             (system repl server)
             (gwwm keymap)
             (gwwm commands)
             (srfi srfi-1)
             (gwwm config))
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

(false-if-exception (spawn-server (make-tcp-server-socket)))

(define (init-global-keybind)
  (keymap-global-set (kbd (s S space))

                     togglefloating)
  (keymap-global-set (kbd (s S C))
                     killclient)

  (keymap-global-set
   (kbd (s f))
   togglefullscreen)
  (keymap-global-set
   (kbd (s e))
   (lambda ()
     (spawn "emacs")))
  (keymap-global-set
   (kbd (s Tab))
   zoom)
  (keymap-global-set
   (kbd (s S Q))
   gwwm-quit)
  (for-each (lambda (a)
              (keymap-global-set
               (kbd* `(C M ,(string->symbol
                             (string-append
                              "XF86Switch_VT_"
                              (number->string a)))))
               (lambda () (chvt a))))
            (iota 12 1)))
