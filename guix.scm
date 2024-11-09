(use-modules
 (guix utils) (guix packages)
 ((guix licenses) #:prefix license:)
 (gnu packages xorg)
 (guix download)
 (guix git-download)
 (gnu packages gettext)
 (guix gexp)
 (gnu packages gl)
 (gnu packages xdisorg)
 (guix build-system gnu)
 (gnu packages bash)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages gtk)
 (gnu packages guile-xyz)
 (gnu packages ibus)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages pciutils)
 (gnu packages wm)
 (guix transformations)
 (gnu packages freedesktop))

(define %srcdir
  (dirname (current-filename)))

(define guile-wlroots
  (primitive-load
   (string-append (dirname (dirname (current-filename)))
                  "/guile-wlroots/guix.scm")))

(define-public gwwm
  (package
    (name "gwwm")
    (version "0.1")
    (source (local-file "." "gwwm-checkout"
                        #:recursive? #t
                        #:select? (git-predicate %srcdir)))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list "GUILE_AUTO_COMPILE=0")
                     ;;; XXX: is a bug? why can't use gexp for #:modules
           #:modules `(((guix build guile-build-system)
                        #:select (target-guile-effective-version))
                       ,@%gnu-build-system-modules)
           #:imported-modules `((guix build guile-build-system)
                                ,@%gnu-build-system-modules)
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'build 'load-extension
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute*
                       (find-files "." ".*\\.scm")
                     (("\\(load-extension \"libgwwm\" *\"(.*)\"\\)" _ o)
                      (string-append
                       (object->string
                        `(or (false-if-exception (load-extension "libgwwm" ,o))
                             (load-extension
                              ,(string-append
                                (assoc-ref outputs "out")
                                "/lib/libgwwm.so")
                              ,o))))))))
               (add-after 'install 'wrap-executable
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (deps (map (lambda (a)
                                       (assoc-ref inputs a ))
                                     '("guile-wayland"
                                       "guile-wlroots"
                                       "guile-bytestructures"
                                       "util572"
                                       "guile-srfi-189"
                                       "guile-srfi-145"
                                       "guile-xkbcommon"
                                       "guile-libinput")))
                          (effective (target-guile-effective-version))
                          (mods (map (lambda (o)
                                       (string-append
                                        o "/share/guile/site/" effective))
                                     (cons out deps)))
                          (gos
                           (map (lambda (o)
                                  (string-append
                                   o "/lib/guile/" effective "/site-ccache"))
                                (cons out deps))))
                     (wrap-program (search-input-file outputs "bin/gwwm")
                       #:sh (search-input-file inputs "bin/bash")
                       `("GUILE_AUTO_COMPILE" ":" = ("0"))
                       `("GUILE_LOAD_PATH" ":" prefix ,mods)
                       `("GUILE_LOAD_COMPILED_PATH" ":" prefix ,gos))))))))
    (native-inputs
     (list autoconf automake
           pkg-config
           libtool
           gettext-minimal
           guile-3.0-latest
           bash-minimal
           texinfo))
    (inputs (list guile-3.0-latest
                  guile-cairo
                  guile-bytestructures
                  guile-srfi-189
                  guile-srfi-145
                  guile-wlroots
                  (lookup-package-input guile-wlroots "wlroots")
                  (lookup-package-input guile-wlroots "util572")
                  (lookup-package-input guile-wlroots "guile-xkbcommon")
                  (lookup-package-input guile-wlroots "guile-libinput")
                  (lookup-package-input guile-wlroots "guile-wayland")))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))
gwwm
