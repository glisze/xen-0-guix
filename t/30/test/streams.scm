;;; stream.scm (C) 2019 GUnter Liszewski                  -*- mode: scheme; -*-
;;; srfi-41 streams

(use-modules (srfi srfi-41))
;; constructor: stream-null
;; constructor: (stream-cons a bs)
;; recognizer: (stream? a)
;; recognizer: (stream-null a)
;; recognizer: (stream-pair? a)
;; accessor: (stream-car as)
;; accessor: (stream-cdr as)
;; lambda: (stream-lambda as b)
