(use-modules (ice-9 getopt-long))
(add-to-load-path (dirname(dirname (current-filename))))
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


(define* (getenv* nam #:optional fallback)
  (or (getenv nam) fallback))

(define (get-xdg-config-home)
  (getenv* "XDG_CONFIG_HOME"
           (string-append (getenv "HOME") "/.config")))

(define (load-init-file)
  (let ((init-file (string-append (get-xdg-config-home) "/gwwm/init.scm")))

    (if (file-exists? init-file)
        (save-module-excursion
         (lambda ()
           (primitive-load
            init-file)))
        (warn (string-append  "initfile not found:" init-file )))
    ))
