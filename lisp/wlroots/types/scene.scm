(define-module (wlroots types scene)
  #:use-module (wayland display)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots utils)
  #:use-module (system foreign)
  #:use-module (oop goops)
  #:export (wrap-wlr-scene
            unwrap-wlr-scene
            wlr-scene-create
            wlr-scene-attach-output-layout
            wlr-scene-node-set-position))

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

(define wlr-scene-attach-output-layout
  (let ((proc (wlr->procedure int "wlr_scene_attach_output_layout" '(* *))))
    (lambda (scene output-layout)
      (proc (unwrap-wlr-scene scene) (unwrap-wlr-output-layout output-layout)))))

(define wlr-scene-node-set-position
  (let ((proc (wlr->procedure int "wlr_scene_node_set_position" (list '* int int))))
    (lambda (scene x y)
      (proc (unwrap-wlr-scene scene) x y))))
