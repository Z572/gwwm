(define-module (gwwm packages fullscreen-bg)
  #:use-module (oop goops)
  #:use-module (srfi srfi-2)
  #:use-module (wlroots types scene)
  #:use-module (util572 color)
  #:use-module (util572 box)
  #:use-module (gwwm client)
  #:use-module (gwwm hooks)
  #:use-module (gwwm config)
  #:export (fullscreen-bg-mode
            fullscreen-bg-color))

(define-once fullscreen-bg-color
  (make-parameter
   (make-rgba-color 0 0 0 255)
   (lambda (new)
     (if (is-a? new <rgba-color>)
         new
         (error "not a <rgba-color> object!" new)))))

(define-once %bgs
  (make-weak-key-hash-table
   ;; random number
   40))

(define-method (client-bg c)
  (hashv-ref %bgs c))
(define-method ((setter client-bg) c bg)
  (hashv-set! %bgs c bg))
(define (remove-bg c)
  (hashv-remove! %bgs c))
(define (for-each-bg f)
  (hash-for-each f %bgs))

(define (add-bg/maybe c)
  (unless (client-bg c)
    (let ((bg (wlr-scene-rect-create
               (.node (client-scene c))
               (box-width (client-geom c))
               (box-height (client-geom c))
               (fullscreen-bg-color))))
      (wlr-scene-node-set-enabled (.node bg) #f)
      (wlr-scene-node-lower-to-bottom (.node bg))
      (set! (client-bg c) bg))))

(define (set-bg c fullscreen?)
  (add-bg/maybe c)
  (let ((bg (client-bg c)))
    (if fullscreen?
        (begin
          (wlr-scene-rect-set-size
           bg
           (box-width (client-geom c))
           (box-height (client-geom c)))
          (wlr-scene-rect-set-color
           bg (fullscreen-bg-color))
          (wlr-scene-node-set-enabled (.node bg) #t))
        (wlr-scene-node-set-enabled (.node bg) #f))))

(define* (fullscreen-bg-mode #:optional (enable? #t) )
  (if enable?
      (begin (add-hook! client-fullscreen-hook set-bg)
             (and-let* ((c (current-client))
                        (fullscreen? (client-fullscreen? c )))
               (set-bg c fullscreen?)))
      (begin (for-each-bg
              (lambda (c bg)
                (when (client-alive? c)
                  (wlr-scene-node-destroy (.node bg)))))
             (remove-hook! client-fullscreen-hook set-bg)))
  *unspecified*)
