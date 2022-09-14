(define-module (gwwm utils)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (ice-9 format)
  #:use-module (gwwm i18n)
  #:export (->symbol
            ->string
            getenv*
            get-xdg-config-home))

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
