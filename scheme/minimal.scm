; minimal.scm - guile code compiled into gwave so its always there.
; Copyright (C) 1999 Steve Tell
; portions from scwm/minimal.scm:
;;;; Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros

(if gwave-debug
      (begin
	(display "in minimal.scm") (newline)))

(debug-enable 'debug)
(read-enable 'positions)

; print list if debug flag is set
(define (dbprint . l)
  (if gwave-debug
      (for-each (lambda (e) (display e (current-output-port))) l)))

; Guile compatibility stuff borrowed from scwm.  Not tested recently.
(if (not (defined? 'run-hook))
    (define-public (run-hook hook-list . args)
      "Runs the procedures in HOOK-LIST, each getting ARGS as their arguments.
If any error, the others still run.  The procedures are executed in the
order in which they appear in HOOK-LIST"
      (for-each (lambda (p) 
		  (catch #t
			 (lambda () (apply p args))
			 (lambda args
			   (display "Error running hook: ")
			   (write p)
			   (newline))))
		hook-list)))


(if (not (defined? 'reset-hook!))
    (defmacro-public reset-hook! (hook)
      `(set! ,hook ())))

(if (not (defined? 'make-hook))
    (begin
      ;; guile-1.3
      (define-public (make-hook . n) ())
      (define-public hook? list?))
    ;; guile-1.3.2 and later
    (define-public (hook? h) 
      (and (pair? h) (eq? (car h) 'hook))))


; TODO: check to see if gwave-guiledir is already present in the load-path,
; and if so, don't add it.
(let* ((genv (getenv "GWAVE_GUILE_DIR"))
       (gwave-guiledir (if genv
			   genv
			   (string-append gwave-datadir "/guile"))))
  (set! %load-path (cons gwave-guiledir %load-path)))

(dbprint "%load-path=" %load-path "\n")
;
; Find a .gwaverc file to load, loading only the first one found.
; I'm not sure this is quite the model I want:
; since the program is pretty useless without getting a bunch of things loaded,
; perhaps they all should be loaded, allowing things to append and override.
; That requires figuring out how to make the stuff in system.gwaverc more
; flexibile though.
;
(let ((home-gwaverc (string-append (getenv "HOME") "/.gwaverc"))
      (system-gwaverc (string-append gwave-datadir
				     "/guile/app/gwave/system.gwaverc")))
  (if (access? "./.gwaverc" R_OK)
      (safe-load "./.gwaverc")
      (if (access? home-gwaverc R_OK)
	  (safe-load home-gwaverc)
	  (if (access? system-gwaverc R_OK)
	      (safe-load system-gwaverc)))))
