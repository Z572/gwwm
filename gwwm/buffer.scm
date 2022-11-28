(define-module (gwwm buffer)
  #:use-module (system foreign-object)
  #:use-module (cairo)
  #:use-module (oop goops)
  #:export (cairo-buffer-create
            cairo-buffer-base
            cairo-buffer-cairo
            .cairo))

;; (use-modules (gwwm buffer)
;;              (oop goops describe)
;;              (gwwm client)
;;              (cairo)
;;              (wlroots types scene))
;; (define buf (cairo-buffer-create 30 40))
;; (define cr (.cairo buf))
;; (define target (cairo-get-target cr))
;; (cairo-set-source-rgba cr 1.0 0.4 1.0 1.0)
;; (cairo-set-fill-rule cr 'winding)
;; (cairo-rectangle cr 0 0 15 40)
;; (cairo-fill cr)
;; (cairo-set-source-rgba cr 0.4 1.0 1.0 1.0)
;; (cairo-rectangle cr 4 0 30 20)
;; (cairo-fill cr)
;; (cairo-surface-flush target)
;; (spawn "emacs")
;; (sleep 2)
;; (wlr-scene-buffer-create (client-scene (car (client-list)))
;;                          (cairo-buffer-base buf))




(eval-when (expand load eval)
  (load-extension "libgwwm" "scm_init_gwwm_buffer"))
(define-class <gwwm-cairo-buffer> ()
  (%data #:init-keyword #:data
         ;; #:accessor .data
         #:class <hidden-slot>)
  (buffer
   #:allocation
   #:virtual
   #:slot-ref cairo-buffer-base
   #:slot-set! set-cairo-buffer-base!
   #:accessor .buffer)
  (image-surface #:accessor .image-surface)

  (cairo #:accessor .cairo)
  (format #:accessor .format))
