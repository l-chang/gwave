;
; module providing gwave's standard command-line argument parsing.
;

(define-module (app gwave std-args)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 common-list)
  :use-module (app gwave cmds)
;  :use-module (app gwave options)
)

(debug-enable 'debug)
(read-enable 'positions)

(dbprint "std-args.scm running\n")

;
; Note: so long as there is still a getopt(3) call in gwave.c, 
; we have to be sure all options are listed both places.
;
; TODO:  better checking of option arguments;
;  make sure numeric ones are numbers, and within valid range. 
;  Clean up arg parsing in gwave.c, don't exit on error.
;  Detect argument errors here and print nice error message
;  coordinate usage messages with C
;
(define opts (getopt-long (program-arguments)
             `((nobacktrace  (single-char #\n))
               (panels       (single-char #\p) (value #t))
	       (verbose      (single-char #\v))
	       (debug        (single-char #\x))
	       )))

(define verbose
  (let ((a (assq 'verbose opts)))
    (if a
        (cdr a)
        #f)))

(define npanels
  (let ((a (assq 'panels opts)))
    (if a
        (string->number (cdr a))
        2)))

(define cmdline-files (pick string? (assq '() opts)))

;(display "opts:") (display opts) (newline)
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
   (do ((i 0 (+ i 1))) ((>= i npanels))
     (wtable-insert-typed-panel! #f default-wavepanel-type))
))


(dbprint "std-args.scm done\n")
