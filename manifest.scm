#!/usr/bin/env bash
exec guix shell -m $0 $@; -*- mode: scheme -*-
!#
(use-modules
 (srfi srfi-1)
 (guix packages)
 (guix profiles)
 (guix transformations))
(primitive-load (string-append (dirname (current-filename)) "/guix.scm"))
(define transform
  (options->transformation
   '((with-debug-info . "wlroots-next,wayland-next,libdrm-next,guile"))))

(packages->manifest
 (append
  (map specification->package
       '("gdb"
         "bear"
         "ccls"))
  (filter
   package?
   (map
    second
    (package-development-inputs (transform gwwm))))))
