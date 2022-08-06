(use-modules (ice-9 getopt-long)
             (system repl server)
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
