(define-module (gwwm buffer)
  #:use-module (system foreign-object)
  #:use-module (cairo)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (wlroots types)
  #:use-module (wlroots types buffer)
  #:use-module (wayland signal)
  #:use-module (wayland list)
  #:use-module (wayland server listener)
  #:use-module (gwwm utils srfi-215)
  #:use-module (oop goops)

  #:export (cairo-buffer-create
            cairo-buffer-base
            cairo-buffer-cairo))

(use-modules (gwwm buffer)
             (oop goops describe)
             (gwwm client)
             (cairo)
             (wlroots types scene))

;; (define (test)
;;   (let* ((buf (cairo-buffer-create 30 40))
;;          (cr (cairo-buffer-cairo buf))
;;          (target (cairo-get-target cr)))
;;     (cairo-set-source-rgba cr 1.0 0.4 1.0 1.0)
;;     (cairo-set-fill-rule cr 'winding)
;;     (cairo-rectangle cr 0 0 15 40)
;;     (cairo-fill cr)
;;     (cairo-set-source-rgba cr 0.4 1.0 1.0 1.0)
;;     (cairo-rectangle cr 4 0 30 20)
;;     (cairo-fill cr)
;;     (cairo-surface-flush target)
;;     (letrec* ((sc-b (wlr-scene-buffer-create (client-scene (car (client-list)))
;;                                              (cairo-buffer-base buf)))
;;               (node (.node sc-b))
;;               (listener (make-wl-listener
;;                          (lambda _
;;                            (wlr-buffer-drop (cairo-buffer-base buf))
;;                            (wl-listener-remove listener)))))
;;       (wl-signal-add (get-event-signal node 'destroy)
;;                      listener))))

(eval-when (expand load eval)
  (load-extension "libgwwm" "scm_init_gwwm_buffer"))

;; (define DRM_FORMAT_ARGB8888 875713089)

;; (define (cairo-buffer-create width height)
;;   (let* ((surface (cairo-image-surface-create 'argb32 width height))
;;          (cairo (cairo-create surface)))
;;     (make <gwwm-cairo-buffer>
;;       #:image-surface surface
;;       #:cairo cairo
;;       #:format DRM_FORMAT_ARGB8888)))

(define-class <gwwm-cairo-buffer> ()
  (%data #:init-keyword #:data))
