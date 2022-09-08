(define-module (gwwm hooks)
  #:export (create-monitor-hook
            create-pointer-hook
            axis-event-hook
            update-title-hook
            selection-hook
            modifiers-event-hook
            keypress-event-hook
            cursor-button-event-hook
            surface-commit-event-hook))

(define-public create-monitor-hook (make-hook 1))
(define-public create-pointer-hook (make-hook 1))
(define-public axis-event-hook (make-hook 1))
(define-public update-title-hook (make-hook 1))
(define-public selection-hook (make-hook 1))
(define-public modifiers-event-hook (make-hook 1))
(define-public keypress-event-hook (make-hook 2))
(define-public cursor-button-event-hook (make-hook 1))
(define-public surface-commit-event-hook (make-hook 1))
