#!/usr/bin/env guile
!#
;;; guix build --file=s/xen-0-tools.scm (c) 2019 Gunter Liszewskl, GPL3
(define (guix-build-xen-0-tools)
  "Invoke GUIX BUILD to make XEN-TOOLS"
  (system* "guix"
	   "build"
           "-k"
	   "--file=s/xen-tools-stable-4.13.scm"))

(guix-build-xen-0-tools) ;; ok, go ahead
