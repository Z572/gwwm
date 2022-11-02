(define-module (gwwm utils)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (ice-9 format)
  #:use-module (gwwm i18n)
  #:export (->symbol
            ->string
            getenv*
            get-xdg-config-home
            string-split-length)
  #:export-syntax (save-environment-excursion
                   with-env
                   let-slots
                   modify-instance
                   modify-instance*))

(define-syntax let-slots
  (lambda (x)
    (syntax-case x ()
      ((_ obj ((slot-name changed-name)) body body* ...)
       #'(let ((changed-name (slot-ref obj 'slot-name )))
           body body* ...))
      ((_ obj (slot-name) body body* ...)
       #'(let ((slot-name (slot-ref obj 'slot-name )))
           body body* ...))
      ((_ obj () body body* ...)
       #'(let () body body* ...))
      ((_ obj (var var* ...) body body* ...)
       #'(let ((obj* obj))
           (let-slots obj* (var)
             (let-slots obj* (var* ...)
               body body* ...)))))))

(define-syntax modify-instance
  (lambda (x)
    (syntax-case x ()
      ((_ obj ((slot-name changed-name) sexp ...) ...)
       #'(let ((obj* obj))
           (let-slots obj* ((slot-name changed-name) ...)
             (slot-set! obj* 'slot-name
                        (begin sexp ...)) ...)))
      ((_ obj (slot-name sexp ...) ...)
       #'(let ((obj* obj))
           (let-slots obj* (slot-name ...)
             (slot-set! obj* 'slot-name
                        (begin sexp ...)) ...))))))

(define-syntax modify-instance*
  (lambda (x)
    (syntax-case x ()
      ((_ obj ((slot-name changed-name) sexp ...) ...)
       #'(let ((obj* obj))
           (let-slots obj* ((slot-name changed-name) ...)
             (let ((out (begin sexp ...)))
               (slot-set! obj* 'slot-name out)
               (set! changed-name out)) ...)))
      ((_ obj (slot-name sexp ...) ...)
       #'(let ((obj* obj))
           (let-slots obj* (slot-name ...)
             (let ((out (begin sexp ...)))
               (slot-set! obj* 'slot-name out)
               (set! slot-name out)) ...))))))
;; (define-syntax-rule (modify-slots))

(define* (string-split-length s length)
  (let loop ((s s))
    (if (> (string-length s ) length)
        (cons (string-take s length) (loop (string-drop s length)))
        (list s))))

;;; copy from guix.
(define-syntax-rule (save-environment-excursion body ...)
  "Save the current environment variables, run BODY..., and restore them."
  (let ((env (environ)))
    (dynamic-wind
      (const #t)
      (lambda ()
        body ...)
      (lambda ()
        (environ env)))))

(define-syntax with-env
  (lambda (x)
    "
(getenv \"HOME\") => \"/root\"
(with-env ((\"HOME\" \"/tmp\"))
  (getenv \"HOME\")) => \"/tmp\"
"
    (syntax-case x ()
      ((_ ((env value) ...) body ...)
       #'(save-environment-excursion
          (setenv env value)
          ...
          body ...)))))



(define* (getenv* nam #:optional fallback)
  "like getenv, but if NAM environment variable not found return FALLBACK."
  (or (getenv nam) fallback))

(define (get-xdg-config-home)
  "return XDG_CONFIG_HOME."
  (getenv* "XDG_CONFIG_HOME"
           (string-append (getenv "HOME") "/.config")))

(define-method (describe (m <hashtable>))
  (format #t (G_ "~S is a hashtable, value is:~%~:{\t~s => ~s\n~}.~%")
          m (hash-map->list
             list m))
  *unspecified*)

(define-method (->symbol (o <symbol>))
  o)
(define-method (->symbol (o <number>))
  (->symbol (number->string o)))

(define-method (->symbol (o <string>))
  (string->symbol o))

(define-method (->symbol (o <keyword>))
  (keyword->symbol o))

(define-method (->string (o <string>))
  o)

(define-method (->string (o <keyword>))
  (->string (keyword->symbol o)))

(define-method (->string (o <symbol>))
  (symbol->string o))

(define-method (->string (o <number>))
  (number->string o))
