(define-module (gwwm hooks)
  #:export (create-monitor-hook
            axis-event-hook
            update-title-hook
            selection-hook))

(define-public keypress-hook (make-hook 2))
(define-public create-monitor-hook (make-hook 1))
(define-public axis-event-hook (make-hook 1))
(define-public update-title-hook (make-hook 1))
(define-public selection-hook (make-hook 1))
(define-public modifiers-event-hook (make-hook 1))
