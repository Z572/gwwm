(define-module (gwwm keybind)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots types keyboard)
  #:use-module (gwwm commands)
  #:use-module (oop goops )
  #:use-module (gwwm keymap)
  #:use-module (ice-9 match))

(define SHIFT WLR_MODIFIER_SHIFT )
(define CAPS  WLR_MODIFIER_CAPS )
(define CTRL  WLR_MODIFIER_CTRL )
(define ALT   WLR_MODIFIER_ALT  )
(define MOD2  WLR_MODIFIER_MOD2 )
(define MOD3  WLR_MODIFIER_MOD3 )
(define LOGO  WLR_MODIFIER_LOGO )
(define MOD5  WLR_MODIFIER_MOD5 )

(define-modify-key 'C CTRL)
(define-modify-key 's LOGO)
(define-modify-key 'S SHIFT)
(define-modify-key 'M ALT)

(define & logand)
(define ^ logxor)
(define ~ lognot)

(define (have-mk mks mk)
  (not (= mks (& mks (~ mk)))))

(define (ref-key k)
  (module-ref (resolve-interface '(gwwm keys))
              (symbol-append 'key- k)))
;; (define-method (ref-key (k <integer>))
;;   (ref-key (string->symbol (number->string k))))

(define (clean-caps mks)
  (& mks (~ WLR_MODIFIER_CAPS)))

(define* (keybinding mods sym #:optional (pressed #t))
  (define handle (match-lambda
                   ((k command release-command)
                    (if (and (= (clean-caps mods)
                                (clean-caps (apply logior (map parse-modify-key (.modify-keys k)))) )
                             (= sym (ref-key (.key k))))
                        (begin (if pressed
                                   (command)
                                   (release-command))
                               #t)
                        #f))))
  (any handle (.keys ((@@ (gwwm) global-keymap)))))
