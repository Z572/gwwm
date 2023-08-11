(define-module (gwwm listener)
  #:use-module (system foreign)
  #:use-module (wlroots types)
  #:use-module (wayland signal)
  #:use-module (wayland list)
  #:use-module (wayland server listener)
  #:use-module (gwwm utils srfi-215)
  #:use-module (oop goops)
  #:export (add-listen))

(define* (add-listen obj symbol proc
                     #:key
                     (destroy-when obj)
                     (remove-when-destroy? #t))
  (let ((listener (make-wl-listener proc)))
    (send-log DEBUG "listener added" 'object obj 'event symbol)
    (wl-signal-add (get-event-signal obj symbol) listener)
    (when remove-when-destroy?
      (wl-signal-add
       (get-event-signal destroy-when 'destroy)
       (make-wl-listener
        (lambda _
          (send-log DEBUG "listener removed" 'object obj 'event symbol)
          (wl-listener-remove listener)))))))
