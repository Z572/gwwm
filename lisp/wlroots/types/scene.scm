(define-module (wlroots types scene)
  #:use-module (wayland display)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module (wayland signal)
  #:use-module (wayland listener)
  #:use-module (wlroots render renderer)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots types)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (rnrs bytevectors)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:export (wrap-wlr-scene
            unwrap-wlr-scene
            wrap-wlr-scene-node
            unwrap-wlr-scene-node
            wrap-wlr-scene-tree
            unwrap-wlr-scene-tree
            wlr-scene-create
            wlr-scene-attach-output-layout
            wlr-scene-node-set-position
            wlr-scene-node-at
            wlr-scene-tree-create
            .node))

(define %wlr-scene-node-state-struct
  (bs:struct `((link ,%wl-list)
               (children ,%wl-list)
               (enabled ,int8)
               (x ,int)
               (y ,int))))
(define %wlr-scene-node-struct
  (bs:struct `((type ,int)
               (parent ,(bs:pointer '*))
               (state ,%wlr-scene-node-state-struct)
               (events
                ,(bs:struct `((destroy ,%wl-signal-struct)))))))
(define %wlr-scene-tree-struct
  (bs:struct `((node ,%wlr-scene-node-struct))))

(define %wlr-scene-struct
  (bs:struct `((node ,%wlr-scene-node-struct)
               (outputs ,%wl-list)
               (presentation ,(bs:pointer '*))
               (presentation-destroy ,%wl-listener)
               (peeding-buffers ,%wl-list))))

(define-wlr-types-class wlr-scene-node)
(define-wlr-types-class wlr-scene-tree)

(define-class <wlr-scene> ()
  (item #:accessor .item #:init-keyword #:item)
  (node #:allocation #:virtual
        #:accessor .node
        #:slot-ref
        (lambda (a)
          (wrap-wlr-scene-node (bytestructure->pointer (bytestructure-ref (.item a) 'node))))
        #:slot-set!
        (lambda (a new-val)
          (bytestructure-set! (.item a) 'node (unwrap-wlr-scene-node new-val)))))

(define-method (.node (o <wlr-scene-tree>))
  (bytestructure-ref (pointer->bytestructure (unwrap-wlr-scene-tree o) %wlr-scene-tree-struct) 'node))
(define (wrap-wlr-scene p)
  (make <wlr-scene>
    #:item
    (pointer->bytestructure p %wlr-scene-struct)))
(define (unwrap-wlr-scene o)
  (bytestructure->pointer (.item o)))

(define-wlr-procedure (wlr-scene-create)
  ('* "wlr_scene_create" '())
  (wrap-wlr-scene (%)))

(define-wlr-procedure (wlr-scene-attach-output-layout scene output-layout)
  (ffi:int "wlr_scene_attach_output_layout" '(* *))
  (% (unwrap-wlr-scene scene) (unwrap-wlr-output-layout output-layout)))

(define-wlr-procedure (wlr-scene-node-set-position scene x y)
  (ffi:void "wlr_scene_node_set_position" (list '* ffi:int ffi:int))
  (% (unwrap-wlr-scene scene) x y))
(define-wlr-procedure (wlr-scene-tree-create parent)
  ('* "wlr_scene_tree_create" '(*))
  (wrap-wlr-scene-tree (% (pk 's (unwrap-wlr-scene-node parent)))))
(define-wlr-procedure (wlr-scene-node-at node lx ly
                                         #:optional
                                         (nx (ffi:bytevector->pointer (make-bytevector (ffi:sizeof '*))))
                                         (ny (ffi:bytevector->pointer (make-bytevector (ffi:sizeof '*)))))
  ('* "wlr_scene_node_at" (list '* ffi:double ffi:double '* '*))
  (values (% (get-pointer node) lx ly nx ny)
          (cons (bytevector-ieee-double-native-ref (ffi:pointer->bytevector nx (ffi:sizeof double)) 0)
                (bytevector-ieee-double-native-ref (ffi:pointer->bytevector ny (ffi:sizeof double)) 0))))
