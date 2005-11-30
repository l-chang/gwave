;
; module providing gwave's standard command-line argument parsing.
;

(define-module (app gwave std-args)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 common-list)
  :use-module (app gwave cmds)
  :use-module (app gwave export)
)

(debug-enable 'debug)
(read-enable 'positions)

(dbprint "std-args.scm running\n")

;
; TODO:  better checking of option arguments;
;  make sure numeric ones are numbers, and within valid range. 
;
(define opts 
  (catch 'misc-error 
	 (lambda () 
	   (getopt-long (program-arguments)
			`((nobacktrace  (single-char #\n))
			  (panels       (single-char #\p) (value #t))
			  (script       (single-char #\s) (value #t))
			  (verbose      (single-char #\v))
			  (debug        (single-char #\x))
			  )))
	 (lambda (key args . rest) 
	   (apply display-error #f (current-error-port) args rest)
					; todo: usage message
	   (exit 1)
	   )))

(define verbose
  (let ((a (assq 'verbose opts)))
    (if a
        (cdr a)
        #f)))

(let ((a (assq 'panels opts)))
    (if a
        (set! initial-panels (string->number (cdr a)))))

(define startup-script #f)
(let ((s (assq 'script opts)))
    (if s
	(if (string? (cdr s))
	    (set! startup-script (cdr s)))))

(define cmdline-files (pick string? (assq '() opts)))

;(display "opts:") (display opts) (newline)
;(display "script:") (display startup-script)(newline)
;(display "args: verbose=")(display verbose)
;(display " npanels=")(display npanels)
;(display " files=")
;(for-each (lambda (f) 
;	    (begin (display f) (display " ")))
;	    cmdline-files)
;(newline)

;
; wave window hook to load files listed on the command line and
; create initial panels.
; We use append-hook so that the menubar is already created; this way
; the view->variable list menu item works properly for these.

(append-hook! 
 new-wavewin-hook
 (lambda ()
   (dbprint "in std-args new-wavewin-hook\n")
   ; load files listed on the command line
   (for-each (lambda (f)
	       (load-wavefile! f)) 
	     cmdline-files)

   ; add the initial set of panels
   (do ((i 0 (+ i 1))) ((>= i initial-panels))
     (wtable-insert-typed-panel! #f default-wavepanel-type))

   ; execute script specified with -s 
   (if startup-script
       (load startup-script))
))


(dbprint "std-args.scm done\n")
