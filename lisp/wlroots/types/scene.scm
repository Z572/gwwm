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

(define-wlr-procedure (wlr-scene-create)
  ('* "wlr_scene_create" '())
  (wrap-wlr-scene (%)))

(define-wlr-procedure (wlr-scene-attach-output-layout scene output-layout)
  (int "wlr_scene_attach_output_layout" '(* *))
  (% (unwrap-wlr-scene scene) (unwrap-wlr-output-layout output-layout)))

(define-wlr-procedure (wlr-scene-node-set-position scene x y)
  (void "wlr_scene_node_set_position" (list '* int int))
  (% (unwrap-wlr-scene scene) x y))
