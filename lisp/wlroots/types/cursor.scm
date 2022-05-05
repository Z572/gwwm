(define-module (wlroots types cursor)
  #:use-module (wayland display)
  #:use-module (wayland signal)
  #:use-module (wlroots types input-device)
  #:use-module (srfi srfi-26)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:select ((void . ffi:void)
                                           (int32 . ffi:int32)
                                           (int . ffi:int)
                                           (double . ffi:double)))
  ;; #:use-module (system foreign)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:export (wrap-wlr-cursor
            unwrap-wlr-cursor
            wlr-cursor-create
            wlr-cursor-attach-output-layout
            %wlr-cursor-struct
            wlr-cursor-set-surface
            wlr-cursor-move
            wlr-cursor-warp-absolute
            wlr-cursor-attach-input-device))

(define %wlr-cursor-struct
  (bs:struct `((state ,(bs:pointer '*))
               (x ,double)
               (y ,double)
               (event ,(bs:struct (map (cut cons <> (list %wl-signal-struct))
                                       '(motion
                                         motion_absolute
                                         button
                                         axis
                                         frame
                                         swipe_begin
                                         swipe_update
                                         swipe_end
                                         pinch_begin
                                         pinch_update
                                         pinch_end
                                         hold_begin
                                         hold_end

                                         touch_up
                                         touch_down
                                         touch_motion
                                         touch_cancel
                                         touch_frame

                                         tablet_tool_axis
                                         tablet_tool_proximity
                                         tablet_tool_tip
                                         tablet_tool_button
                                         ))))
               (data ,(bs:pointer 'void)))))
(define-wlr-types-class wlr-cursor)
(define-wlr-procedure (wlr-cursor-create)
  ('* "wlr_cursor_create" '())
  (wrap-wlr-cursor (%)))

(define-wlr-procedure (wlr-cursor-attach-output-layout cursor output-layout)
  (ffi:int "wlr_cursor_attach_output_layout" '(* *))
  (% (unwrap-wlr-cursor cursor)
     (unwrap-wlr-output-layout output-layout)))
(define-wlr-procedure (wlr-cursor-set-surface cur surface hostpot-x hostpot-y)
  (ffi:void "wlr_cursor_set_surface" `(* * ,ffi:int32 ,ffi:int32))
  (% (unwrap-wlr-cursor cur)
     (unwrap-wlr-surface surface)
     hostpot-x
     hostpot-y))

(define-wlr-procedure (wlr-cursor-move cur dev delta-x delta-y)
  (ffi:void "wlr_cursor_move" `(* * ,ffi:double ,ffi:double))
  (% (unwrap-wlr-cursor cur)
     (unwrap-wlr-input-device dev)
     delta-x
     delta-y))


(define-wlr-procedure (wlr-cursor-warp-absolute cur dev x y)
  (ffi:void "wlr_cursor_warp_absolute" `(* * ,ffi:double ,ffi:double))
  (% (unwrap-wlr-cursor cur)
     (unwrap-wlr-input-device dev)
     x
     y))
(define-wlr-procedure (wlr-cursor-attach-input-device cur dev)
  (ffi:void "wlr_cursor_attach_input_device" '(* *))
  (% (unwrap-wlr-cursor cur) (unwrap-wlr-input-device dev)))
