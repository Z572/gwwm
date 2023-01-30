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

(define (get-slot-getter obj sym)
  (let* ((class (class-of obj))
         (def (class-slot-definition class sym))
         (get (or (and=> (slot-definition-getter def)
                         (lambda (gett)
                           (lambda ()(gett obj))))
                  (and=> (slot-definition-accessor def)
                         (lambda (gett)
                           (lambda () (gett obj))))
                  (lambda () (slot-ref obj sym)))))
    get))

(define (get-slot-set obj sym)
  (let* ((class (class-of obj))
         (def (class-slot-definition class sym))
         (set (or (and=> (slot-definition-setter def)
                         (lambda (set-f)
                           (lambda (var)
                             (set-f obj var))))
                  (and=> (slot-definition-accessor def)
                         (lambda (set-f)
                           (lambda (var)
                             (set! (set-f obj) var))))
                  (lambda (val) (slot-set! obj sym val)))))
    set))

(define-syntax let-slots
  (lambda (x)
    (syntax-case x ()
      ((_ obj (slot ...) body body* ...)
       (let* ((slots (map (lambda (o)
                            (syntax-case o ()
                              ((slot-name changed-name)
                               #'(slot-name changed-name))
                              (slot-name
                               #'(slot-name slot-name))))
                          #'(slot ...))))
         (syntax-case slots ()
           (((name changed) ...)
            (with-syntax (((%get ...) (generate-temporaries #'(name ...)))
                          ((%set ...) (generate-temporaries #'(name ...))))
              #`(let ((%obj obj))
                  (let ((%get (get-slot-getter %obj 'name)) ...
                        (%set (get-slot-set %obj 'name)) ...)
                    (letrec-syntax
                        ((changed
                          (identifier-syntax
                           (var (%get))
                           ((set! var val)
                            (%set val)))) ...)
                      body body* ...)))))))))))

(define-syntax modify-instance
  (lambda (x)
    (syntax-case x ()
      ((_ obj ((slot-name changed-name) sexp ...) ...)
       #'(let ((obj* obj))
           (let-slots obj* ((slot-name changed-name) ...)
             (set! slot-name
                   (begin sexp ...)) ...)))
      ((_ obj (slot-name sexp ...) ...)
       #'(let ((obj* obj))
           (let-slots obj* (slot-name ...)
             (set! slot-name
                   (begin sexp ...)) ...))))))

(define-syntax modify-instance*
  (lambda (x)
    (syntax-case x ()
      ((_ obj ((slot-name changed-name) sexp ...) ...)
       #'(let ((obj* obj))
           (let-slots obj* ((slot-name changed-name) ...)
             (let ((out (begin sexp ...)))
               (set! changed-name out)) ...)))
      ((_ obj (slot-name sexp ...) ...)
       #'(let ((obj* obj))
           (let-slots obj* (slot-name ...)
             (let ((out (begin sexp ...)))
               (set! slot-name out)) ...))))))

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
