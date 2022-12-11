(define-module (gwwm hooks)
  #:export (create-keyboard-hook
            create-client-hook
            create-monitor-hook
            create-pointer-hook
            axis-event-hook
            update-title-hook
            selection-hook
            modifiers-event-hook
            keypress-event-hook
            cursor-button-event-hook
            surface-commit-event-hook
            fullscreen-event-hook
            gwwm-cleanup-hook
            client-map-event-hook
            cursor-frame-event-hook
            create-popup-hook))

(define-public create-monitor-hook (make-hook 1))
(define-public create-client-hook (make-hook 1))
(define-public create-pointer-hook (make-hook 1))
(define-public create-popup-hook (make-hook 1))
(define-public create-keyboard-hook (make-hook 1))
(define-public axis-event-hook (make-hook 1))
(define-public update-title-hook (make-hook 1))
(define-public selection-hook (make-hook 1))
(define-public fullscreen-event-hook (make-hook 2))
(define-public modifiers-event-hook (make-hook 1))
(define-public keypress-event-hook (make-hook 2))
(define-public cleanup-keyboard-hook (make-hook 2))
(define-public cursor-frame-event-hook (make-hook 1))
(define-public cursor-button-event-hook (make-hook 1))
(define-public surface-commit-event-hook (make-hook 1))
(define-public gwwm-cleanup-hook (make-hook 0))
(define-public client-map-event-hook (make-hook 2))
