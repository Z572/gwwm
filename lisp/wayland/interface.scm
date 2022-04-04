(define-module (wayland interface)
  #:use-module (srfi srfi-9)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:use-module ((system foreign) #:select (make-pointer))
  #:export (%wl-message
            %wl-interface
            pointer->wl-interface
            wl-interface-name
            wl-interface-version
            wl-interface-method-count
            wl-interface-methods
            wl-interface-event-count
            wl-interface-events
            wl-interface->pointer

            wl-message-name
            wl-message-signature
            wl-message-types))

(define-record-type <wl-message>
  (%make-wl-message bytestructure)
  wl-message?
  (bytestructure wl-message-bytestructure))

(define %wl-message
  (bs:struct
   `((name ,cstring-pointer)
     (signature ,cstring-pointer)
     (types ,(bs:pointer '*)))))

(define %wl-interface
  (bs:struct
   `((name ,cstring-pointer)
     (version ,int)
     (method-count ,int)
     (methods ,(bs:pointer %wl-message))
     (event-count ,int)
     (events ,(bs:pointer %wl-message)))))

(define (pointer->wl-interface pointer)
  (pointer->bytestructure pointer %wl-interface))

(define (wl-interface-name interface)
  (bytestructure-ref interface 'name))

(define (wl-interface-version interface)
  (bytestructure-ref interface 'version))

(define (wl-interface-method-count interface)
  (bytestructure-ref interface 'method-count))

(define (wl-interface-methods interface)
  (let ((count (wl-interface-method-count interface)))
    (if (zero? count)
        '()
        (let ((methods (pointer->bytestructure (make-pointer (bytestructure-ref interface 'methods))
                                               (bs:vector count %wl-message))))
          (let loop ((l '())
                     (num (- count 1)))
            (if (< num 0)
                l
                (loop (cons (%make-wl-message (bytestructure-ref methods num)) l)
                      (- num 1))))))))

(define (wl-interface-event-count interface)
  (bytestructure-ref interface 'event-count))

(define (wl-interface-events interface)
  (let ((count (wl-interface-event-count interface)))
    (if (zero? count)
        '()
        (let ((events (pointer->bytestructure (make-pointer (bytestructure-ref interface 'events))
                                              (bs:vector count %wl-message))))
          (let loop ((l '())
                     (num (- count 1)) )
            (if (< num 0)
                l
                (loop (cons (%make-wl-message (bytestructure-ref events num)) l)
                      (- num 1))))))))

(define (wl-interface->pointer interface)
  (bytestructure->pointer interface))

(define (wl-message-name message)
  (bytestructure-ref (wl-message-bytestructure message) 'name))

(define (wl-message-signature message)
  (bytestructure-ref (wl-message-bytestructure message) 'signature))

(define (wl-message-types message)
  (bytestructure-ref (wl-message-bytestructure message) 'types))

;;(define wl-interface-name)
