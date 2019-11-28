#!/bin/sh
# two
exec guile -e main -s "$0"  ${1+"$@"}
!#
(define (plan)
	(display "1..1")(newline))
(define (t1)
	(display "ok")(newline))
(define (main as)
	(plan)
	(t1)
	(write as)(newline))
