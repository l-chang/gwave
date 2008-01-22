; minimal.scm - guile code compiled into gwave so its always there.
; Copyright (C) 1999 Steve Tell
; portions from scwm/minimal.scm:
;;;; Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros

(if gwave-debug
      (begin
	(display "in minimal.scm") (newline)))

(debug-enable 'debug)
(read-enable 'positions)

(define startup-error-flag #f)
; TODO: replace this with a catch around the next stuff
;(add-hook! 
; error-hook
; (lambda (a b c d e)
;   (set! startup-error-flag #t)))

; print list if debug flag is set
(define (dbprint . l)
  (if gwave-debug
      (for-each (lambda (e) (display e (current-output-port))) l)))


; TODO: check to see if gwave-guiledir is already present in the load-path,
; and if so, don't add it.
(let* ((genv (getenv "GWAVE_GUILE_DIR"))
       (gwave-guiledir (if genv
			   genv
			   (string-append gwave-datadir "/guile"))))
  (set! %load-path (cons gwave-guiledir %load-path)))

(dbprint "%load-path=" %load-path "\n")

; Do setup and find a .gwaverc
(load-from-path "app/gwave/gwave-startup.scm")

(dbprint "minimal.scm load done flag=" startup-error-flag "\n")

; this next expression must be the last one, returning #t or #f, which becomes
; the return value of the call to scwm_safe_eval_str() in gwave.c
(if startup-error-flag
    (begin
      (display "gwave: Error(s) in gwave-startup.scm or other startup files\n")
      (display "gwave: %load-path was ")(display %load-path)(newline)
      #f)
    #t)
