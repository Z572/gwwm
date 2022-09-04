(define-module (gwwm i18n)
  #:use-module (srfi srfi-26)
  #:export (G_
            N_
            P_
            %gettext-domain))
(define %gettext-domain
  ;; Text domain for strings used in the tools.
  "gwwm")

(define G_ (cut gettext <> %gettext-domain))
(define N_ (cut ngettext <> <> <> %gettext-domain))
