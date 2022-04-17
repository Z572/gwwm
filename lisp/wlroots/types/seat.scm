(define-module (wlroots types seat)
  #:use-module (wayland list)
  #:use-module (wayland display)
  #:use-module (wayland signal)
  #:use-module (wayland listener)
  #:use-module (srfi srfi-26)
  ;; #:use-module (wlroots render renderer)
  ;; #:use-module (wlroots types output-layout)
  #:use-module (wlroots types)
  #:use-module (wlroots types data-device)
  #:use-module (wlroots types surface)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select ((uint32 . ffi:uint32)
                                           (int32 . ffi:int32)
                                           (float . ffi:float)
                                           ;(int . ffi:int)
                                           (void . ffi:void)
                                           (double . ffi:double)
                                           pointer-address
                                           %null-pointer
                                           string->pointer))
  #:use-module (oop goops)
  #:duplicates (merge-generics)
  #:export (wrap-wlr-seat
            unwrap-wlr-seat
            wlr-seat-create
            wlr-seat-pointer-notify-frame
            WLR_POINTER_BUTTONS_CAP
            %wlr-seat-request-set-selection-event-struct
            wrap-wlr-seat-request-set-selection-event
            unwrap-wlr-seat-request-set-selection-event
            %wlr-seat-request-set-cursor-event-struct
            wrap-wlr-seat-pointer-request-set-cursor-event
            unwrap-wlr-seat-pointer-request-set-cursor-event
            wlr-seat-set-selection
            %wlr-seat-struct
            %wlr-seat-client-struct
            wrap-wlr-seat-client
            unwrap-wlr-seat-client
            wlr-seat-pointer-notify-axis))

(define WLR_POINTER_BUTTONS_CAP 16)
(define %wlr-serial-range-struct
  (bs:struct `((min-incl ,uint32)
               (max-incl ,uint32))))

(define %wlr-serial-ringset
  (bs:struct `((data ,%wlr-serial-range-struct)
               (end ,int)
               (count ,int))))
(define %wlr-seat-client-struct
  (bs:struct `((client ,(bs:pointer '*))
               (seat ,(bs:pointer (delay %wlr-seat-struct)))
               (link ,%wl-list)
               (resources ,%wl-list)
               (pointers ,%wl-list)
               (keyboards ,%wl-list)
               (touches ,%wl-list)
               (data-devices ,%wl-list)
               (events ,(bs:struct `((destroy ,%wl-signal-struct))))
               (serials ,%wlr-serial-ringset)
               (needs-touch-frame ,int))))

(define-wlr-types-class wlr-seat-client)
(define %wlr-seat-pointer-state-struct
  (bs:struct `((seat ,(bs:pointer (delay %wlr-seat-struct)))
               (focused-client ,(bs:pointer '*))
               (focused-surface ,(bs:pointer '*))
               (sx ,double)
               (sy ,double)
               (grab ,(bs:pointer '*))
               (default-grab ,(bs:pointer '*))
               (sent-axis-source ,int)
               (cached-axis-source ,int)
               (buttons ,(bs:vector WLR_POINTER_BUTTONS_CAP uint32))
               (button-count ,size_t)
               (grab-button ,uint32)
               (grab-serial ,uint32)
               (grab-time ,uint32)
               (surface-destroy ,%wl-listener)
               (events ,(bs:struct `((focus-change ,%wl-listener)))))))
(define %wlr-seat-keyboard-state-struct
  (bs:struct `((seat ,(bs:pointer (delay %wlr-seat-struct)))
               (keyboard ,(bs:pointer '*))
               (focused-client ,(bs:pointer %wlr-seat-client-struct))
               (focused-surface ,(bs:pointer '*))
               (keyboard-destroy ,%wl-listener)
               (keyboard-keymap ,%wl-listener)
               (keyboard-repeat-info ,%wl-listener)
               (surface-destroy ,%wl-listener)
               (grab ,(bs:pointer '*))
               (default-grab ,(bs:pointer '*))
               (events ,(bs:struct `((focus-change ,%wl-listener)))))))
(define %wlr-seat-touch-state-struct
  (bs:struct `((seat ,(bs:pointer '*))
               (touch-points ,%wl-list)
               (grab-serial ,uint32)
               (grab-id ,uint32)
               (grab ,(bs:pointer '*))
               (default-grab ,(bs:pointer '*)))))
(define %wlr-seat-request-set-selection-event-struct
  (bs:struct `((source ,(bs:pointer '*))
               (serial ,uint32))))

(define-wlr-types-class wlr-seat-request-set-selection-event)

(define %wlr-seat-request-set-cursor-event-struct
  (bs:struct `((seat-client ,(bs:pointer %wlr-seat-client-struct))
               (surface ,(bs:pointer %wlr-surface-struct))
               (serial ,uint32)
               (hostpot-x ,int32)
               (hostpot-y ,int32))))

(define-wlr-types-class wlr-seat-pointer-request-set-cursor-event)

(define %wlr-seat-struct
  (bs:struct `((global ,(bs:pointer '*))
               (display ,(bs:pointer '*))
               (clients ,%wl-list)
               (name ,cstring-pointer)
               (capabilities ,uint32)
               (accumulated-capabilities ,uint32)
               (last-event ,(bs:struct `((tv-sec ,long)
                                         (tv-nsec ,long))))
               (selection-source ,(bs:pointer '*))
               (selection-serial ,uint32)
               (selection-offers ,%wl-list)
               (primary-selection-source ,(bs:pointer '*))
               (primary-selection-serial ,uint32)
               (drag ,(bs:pointer '*))
               (drag-source ,(bs:pointer '*))
               (drag-serial ,uint32)
               (drag-offers ,%wl-list)
               (pointer-state ,%wlr-seat-pointer-state-struct)
               (keyboard-state ,%wlr-seat-keyboard-state-struct)
               (touch-state ,%wlr-seat-touch-state-struct)
               (display-destroy ,%wl-listener)
               (selection-source-destroy ,%wl-listener)
               (primary-selection-source-destroy ,%wl-listener)
               (drag-source-destroy ,%wl-listener)
               (event ,(bs:struct (map (cut cons <> (list %wl-signal-struct))
                                       '(pointer-grab-begin
                                         pointer-grab-end
                                         keyboard-grab-begin
                                         keyboard-grab-end
                                         touch-grab-begin
                                         touch-grab-end
                                         request-set-cursor

                                         request-set-selection

                                         set-selection

                                         request-set-primary-selection

                                         set-primary-selection

                                         request-start-drag
                                         start-drag

                                         destroy))
                                  ))
               (data ,(bs:pointer 'void)))))


(define-wlr-types-class wlr-seat)

(define-wlr-procedure (wlr-seat-create display name)
  ('* "wlr_seat_create" '(* *))
  (wrap-wlr-seat
   (% (unwrap-wl-display display)
      (string->pointer name ))))
(define-wlr-procedure (wlr-seat-pointer-notify-frame seat)
  (ffi:void "wlr_seat_pointer_notify_frame" '(*))
  (% (unwrap-wlr-seat seat)))
(define-wlr-procedure (wlr-seat-set-selection seat source serial)
  (ffi:void "wlr_seat_set_selection" `(* * ,ffi:uint32))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-data-source source) serial ))
(define-wlr-procedure (wlr-seat-pointer-notify-axis wlr-seat time-msec orientation value value-discrete source)
  (ffi:void "wlr_seat_pointer_notify_axis" (list '* ffi:uint32 ffi:int ffi:double ffi:int32 ffi:int))
  (% (unwrap-wlr-seat wlr-seat) time-msec orientation value value-discrete source))
