(define-module (gwwm hooks)
  #:export (create-monitor-hook))

(define-public keypress-hook (make-hook 2))
(define-public create-monitor-hook (make-hook 1))
