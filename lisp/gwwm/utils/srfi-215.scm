;; -*-scheme-*-
;; © 2020 Göran Weinholt
;; © 2022 Zheng Junjie

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice (including the
;; next paragraph) shall be included in all copies or substantial
;; portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-module (gwwm utils srfi-215)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-35)
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:export
  (send-log
   current-log-fields
   current-log-callback
   EMERGENCY ALERT CRITICAL ERROR WARNING NOTICE INFO DEBUG))
;; use (ice-9 q) replace srfi-215 example's queue.

;; These severities are from RFC 5424 ("The Syslog Protocol").
(define EMERGENCY 0)                ; system is unusable
(define ALERT 1)                    ; action must be taken immediately
(define CRITICAL 2)                 ; critical conditions
(define ERROR 3)                    ; error conditions
(define WARNING 4)                  ; warning conditions
(define NOTICE 5)                   ; normal but significant condition
(define INFO 6)                     ; informational messages
(define DEBUG 7)                    ; debug-level messages

(define (field-list->alist plist)
  (let f ((fields plist))
    (cond ((null? fields)
           '())
          ((or (not (pair? fields)) (not (pair? (cdr fields))))
           (error "short field list" plist))
          (else
           (let ((k (car fields)) (v (cadr fields)))
             (if (not v)
                 (f (cddr fields))
                 (let ((k^ (cond ((symbol? k) k)
                                 (else
                                  (error "invalid key" k plist))))
                       (v^ (cond ((string? v) v)
                                 ((and (integer? v) (exact? v)) v)
                                 ((bytevector? v) v)
                                 ((condition? v) v)
                                 ((error-object? v) v) ;R7RS
                                 (else
                                  (let ((p (open-output-string)))
                                    (write v p)
                                    (get-output-string p))))))
                   (cons (cons k^ v^)
                         (f (cddr fields))))))))))

(define current-log-fields
  (make-parameter '()
                  (lambda (plist)
                    (field-list->alist plist)
                    plist)))

(define current-log-callback
  (let ((num-pending-logs 0)
        (pending-logs (make-q)))
    (make-parameter (lambda (log-entry)
                      (enq! pending-logs log-entry)
                      (if (eqv? num-pending-logs 100)
                          (q-pop! pending-logs)
                          (set! num-pending-logs (+ num-pending-logs 1))))
                    (lambda (hook)
                      (unless (procedure? hook)
                        (error "current-log-hook: expected a procedure" hook))
                      (let ((q pending-logs))
                        (set! num-pending-logs 0)
                        (set! pending-logs (make-q))
                        (let lp ()
                          (unless (q-empty? q)
                            (hook (q-pop! q))
                            (lp))))
                      hook))))

;; Send a log entry with the given severity and message. This
;; procedure also takes a list of extra keys and values.
(define (send-log severity message . plist)
  (unless (and (exact? severity) (integer? severity) (<= 0 severity 7))
    (error "send-log: expected a severity from 0 to 7"
           severity message plist))
  (unless (string? message)
    (error "send-log: expected message to be a string"
           severity message plist))
  (let* ((fields (append plist (current-log-fields)))
         (alist (field-list->alist fields)))
    ((current-log-callback) `((SEVERITY . ,severity)
                              (MESSAGE . ,message)
                              ,@alist))))
