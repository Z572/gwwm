(define-module (gwwm config)
  #:use-module (xkbcommon xkbcommon)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (gwwm utils)
  #:use-module (gwwm i18n)
  #:use-module (gwwm color)
  #:use-module (srfi srfi-189)
  #:use-module (gwwm utils srfi-215)
  #:use-module (ice-9 optargs)
  #:export (gwwm
            load-init-file
            init-file
            make-xkb-rules
            get-option-value*
            get-option-value
            get-option-default-value*)
  #:export-syntax (define-config-option))

(define-once %config-options (make-object-property))
(define-syntax define-config-option
  (lambda (x)
    (syntax-case x ()
      ((_ name default-value doc rest ...)
       (and (symbol? (syntax->datum #'name))
            (string? (syntax->datum #'doc)))
       #`(begin
           (define-once name
             (if (%config-options 'name)
                 (begin (send-log INFO (simple-format #f "option `~a' exists, use it."
                                                      'name)
                                  'option 'name)
                        (get-keyword #:parameter (%config-options 'name)))
                 (let-keywords (list rest ...)
                     #t ((conv identity))
                   (let ((o
                          (make-parameter
                           default-value
                           (lambda (x)
                             (let ((v (conv x)))
                               (send-log
                                DEBUG
                                (simple-format
                                 #f "option `~a' value to `~a', converted value is `~a'"
                                 'name x v)
                                'option 'name 'value x 'converted-value v)
                               v)))))
                     (set! (%config-options 'name)
                           (list #:parameter o
                                 #:default-value default-value
                                 #:doc doc))
                     o))))
           (export name))))))

(define-syntax-rule (get-option-value name)
  (get-option-value* 'name))

(define (get-option-value* name)
  (let-keywords (or (%config-options name) '())
      #t
      ((parameter #f))
    (if parameter
        (just (parameter))
        (nothing))))

(define (get-option-default-value* name)
  (let ((o (%config-options name)))
    (if o
        (right (get-keyword #:default-value o))
        (left (simple-format #f "option `~a' no exits." name)))))

(define-config-option enable-xwayland? #f
  "if set to #t, enable xwayland.")

(define-config-option borderpx 1
  "if set to #t, enable xwayland.")

(define (init-file)
  "return init file."
  (string-append
   (get-xdg-config-home)
   "/gwwm/init.scm"))

(define (load-init-file)
  (let ((init-file (init-file)))
    (if (file-exists? init-file)
        (save-module-excursion
         (lambda ()
           (primitive-load init-file)))

        (send-log INFO (simple-format #f (G_ "initfile not found: ~a") init-file)))))

(define* (make-xkb-rules
          #:optional
          (layout #f)
          (variant #f)
          (rules #f)
          #:key
          (model "")
          (options '()))
  (make <xkb-rule-names>
    #:model model
    #:layout layout
    #:variant variant
    #:rules rules
    #:options (string-join options ",")))

(define-config-option xkb-rules (make-xkb-rules)
  "")

(define-config-option repeat-rate 25
  "")

(define-config-option bordercolor (make-color "#ffbbeeff")
  "")

(define-config-option sloppyfocus? #t "")
(define-config-option focuscolor (make-color 255 0 0 255) "")
(define-config-option cursor-normal-image "right_ptr" "")
(define-config-option lockfullscreen? #t "")

(define-config-option quit-when-no-monitor? #t
  "if set to #t, when no monitor found, quit gwwm"
  #:conv ->bool)

(define-syntax-rule (gwwm (init value) ...)
  (begin (init value) ...))
