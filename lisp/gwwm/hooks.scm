(define-module (gwwm hooks)
  #:export (create-monitor-hook
            axis-event-hook))

(define-public keypress-hook (make-hook 2))
(define-public create-monitor-hook (make-hook 1))
(define-public axis-event-hook (make-hook 1))
