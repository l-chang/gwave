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

; Do setup and find a .gwaverc
(load-from-path "app/gwave/gwave-startup.scm")

