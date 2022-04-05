(define-module (wlroots types scene)
  #:use-module (wayland display)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:export (wrap-wlr-scene
            unwrap-wlr-scene
            wlr-scene-create))

(define-class <wlr-scene> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define (wrap-wlr-scene p)
  (make <wlr-scene> #:pointer p))
(define (unwrap-wlr-scene o)
  (.pointer o))

(define wlr-scene-create
  (let ((proc (wlr->procedure '* "wlr_scene_create" '())))
    (lambda ()
      (wrap-wlr-scene (proc)))))
