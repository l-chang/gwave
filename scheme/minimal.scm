; minimal.scm - guile code compiled into gwave so its always there.
; Copyright (C) 1999 Steve Tell
; portions from scwm/minimal.scm:
;;;; Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros

(display "in minimal.scm") (newline)

(debug-enable 'debug)
(read-enable 'positions)

(if (not (defined? 'run-hook))
    ;; GJB:FIXME:MS: I'd like a backtrace when a hook fails
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

; 
; use-gwave-modules facility; copied and renamed from scwm.
; tries to keep going if there are errors.
;
;; GJB:FIXME:: this should not be public,
;; but I leave it public for now for easier debugging --07/03/99 gjb
(define-public *gwave-modules* '())

(define-public (gwave-module-loaded? module)
  "Return #t iff MODULE has been loaded."
  (let ((entry (assoc module *gwave-modules*))) 
    (and entry (null? (cdr entry)))))

(define (use-gwave-module-note-success module)
  (let ((entry (assoc module *gwave-modules*)))
    (if (not entry)
	(set! *gwave-modules* (cons (cons module '()) *gwave-modules*))
	(let ((eval-after-load-proc (cdr entry)))
	  (if (not (null? eval-after-load-proc))
	      (let ((answer (eval-after-load-proc)))
		(set-cdr! entry '())
		answer))))))

(define-public (eval-after-load module proc)
  "Run PROC after MODULE is loaded.
Run PROC immediately if MODULE has already been loaded."
  (if (gwave-module-loaded? module)
      (proc)
      (set! *gwave-modules* (cons (cons module proc) *gwave-modules*))))

(define (process-use-gwave-module module)
  (if (symbol? module)
      (set! module (append '(app gwave) (list module))))
  (catch #t
	 (lambda ()
	   (process-use-modules (list module))
	   (use-gwave-module-note-success module)
	   (run-hook load-processing-hook -1)
	   module)
	 (lambda (key . args)
	   (display "Error loading module: ")
	   (display module) (newline)
	   (catch #t
		  (lambda () 
		    (apply handle-system-error (cons key args))
		    (display "attempting backtrace\n")
		    (backtrace))
		  (lambda (key . args) #t))
	   #f)))

; print list if debug flag is set
(define (dbprint . l)
  (if gwave-debug
      (for-each (lambda (e) (display e (current-output-port))) l)))

(define-public (process-use-gwave-modules module-list)
  "Returns a list of all the modules loaded in successfully.
Modules that failed to load have #f in their place in the
list instead of the module."
  (map process-use-gwave-module (reverse module-list)))

(defmacro use-gwave-modules modules
  `(process-use-gwave-modules ',modules))

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
