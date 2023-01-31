
(use-modules (guix profiles))
(use-modules
 (srfi srfi-1)
 (guix packages)
 (guix profiles)
 (guix transformations))

(concatenate-manifests (list (specifications->manifest
                              (list "gdb" "guile:debug"))

                             (package->development-manifest
                              (primitive-load (string-append (dirname (current-filename))"/guix.scm") ))))
