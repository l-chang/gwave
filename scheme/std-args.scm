;
; module providing gwave's standard command-line argument parsing.
;

(define-module (app gwave std-args)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 common-list)
  :use-module (app gwave cmds)
  :use-module (app gwave options)
)

(debug-enable 'debug)
(read-enable 'positions)

(display "std-args.scm running\n")

; so long as there is still a getopt(3) call in gwave.c, 
; be sure all options are listed both places.
;
; TODO: better checking of option arguments;
;    make sure numeric ones are numbers, and within valid range. 
(define opts (getopt-long (program-arguments)
             `((fillpanels   (single-char #\f))
	       (nobacktrace  (single-char #\n))
               (panels       (single-char #\p) (value #t))
               (filetype     (single-char #\t) (value #t))
	       (verbose      (single-char #\v))
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

(add-hook! 
 new-wavewin-hook
 (lambda ()
   (display "in std-args new-wavewin-hook\n")
   ; load files listed on the command line
   (for-each (lambda (f)
	       (load-wavefile! f)) cmdline-files)

   ; add the initial set of panels
   (do ((i 0 (+ i 1))) ((>= i npanels))
     (wtable-insert-panel! #f default-wavepanel-type))
))



(display "std-args.scm done\n")
