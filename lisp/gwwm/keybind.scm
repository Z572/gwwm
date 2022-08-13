(define-module (gwwm keybind)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots types keyboard)
  #:use-module (gwwm commands)
  #:use-module (oop goops )
  #:use-module (gwwm keymap)
  #:use-module (ice-9 match)
  ;; #:export (;;ref-key
  ;;           ;; SHIFT
  ;;           ;; CAPS
  ;;           ;; CTRL
  ;;           ;; ALT
  ;;           ;; MOD2
  ;;           ;; MOD3
  ;;           ;; LOGO
  ;;           ;; MOD5
  ;;           )
  )

(define* (parse-modify-key m #:optional (error-when-no-found? #t))
  ((@@ (gwwm keymap) parse-modify-key)
   m error-when-no-found?))

(define & logand)
(define ^ logxor)
(define ~ lognot)

(define (have-mk mks mk)
  (not (= mks (& mks (~ mk)))))

(define (ref-key k)
  (module-ref (resolve-interface '(gwwm keys))
              (symbol-append 'XKB_KEY_ k)))

(define (clean-caps mks)
  (& mks (~ WLR_MODIFIER_CAPS)))

(define (keybinding mods sym)
  (define handle (match-lambda
                   ((k command)
                    (if (and (= (clean-caps mods)
                                (clean-caps (apply logior (map parse-modify-key (.modify-keys k)))) )
                             (= sym (ref-key (.key k))))
                        (begin (command) #t)
                        #f))))
  (any handle (.keys (global-keymap))))
