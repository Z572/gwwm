(define-module (gwwm listener)
  #:use-module (system foreign)
  #:use-module (wlroots types)
  #:use-module (wayland signal)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (oop goops)
  #:export (add-listen))

(define* (add-listen obj symbol proc
                     #:key
                     (destroy-when obj)
                     (remove-when-destroy? #t))
  (let ((listener (make-wl-listener proc)))
    (wl-signal-add (get-event-signal obj symbol) listener)
    (when remove-when-destroy?
      (wl-signal-add
       (get-event-signal destroy-when 'destroy)
       (make-wl-listener
        (lambda _
          (wl-list-remove (.link listener))))))))
