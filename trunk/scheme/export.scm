;
; module providing gwave commands and dialogs for exporting data
;

(define-module (app gwave export)
  :use-module (gtk gtk)
  :use-module (ice-9 optargs)
  :use-module (app gwave cmds)
  :use-module (app gwave gtk-helpers)
)
(read-set! keywords 'prefix)
(debug-enable 'backtrace)
(debug-enable 'debug)

; list of registered plot filters.  each entry is a 3-element list:
; (name dialog-builder-procedure export-procedure)
(define plot-list '())

(define-public (register-plotfilter name dproc eproc)
  (set! plot-list (cons 
		  (list name dproc eproc #f)
		  plot-list)))

 ; debug: dump export filter list
(define (dump-plotf-list)
    (format #t "plot-list:\n")
    (for-each (lambda (exptype)
		(format #t " name=~s " (car exptype))
		(format #t "dproc=~s\n" (cadr exptype))
		(format #t " eproc=~s\n" (caddr exptype)))
	      plot-list))

(define (export-variables-to-file f vwlist . ext)
 (let ((p (open f (logior O_WRONLY O_CREAT O_TRUNC) #o0777)))
   (print "ext is " ext "\n");
   (if (null? ext)
	  (export-variables vwlist p)
	  (export-variables vwlist p (car ext) (cadr ext)))
   (close-port p)
   ))

(define-public (popup-export-dialog wvlist)
  (let* ((window (gtk-window-new 'toplevel))
	 (vbox (gtk-vbox-new #f 0))
	 (hbox1 (gtk-hbox-new #f 10))
	 (outf-label (gtk-label-new "Output File:"))
	 (filename-entry (gtk-entry-new))
	 (browse-btn (gtk-button-new-with-label "Browse"))
	 
	 (hbox2 (gtk-hbox-new #f 10))
	 (use-extents #f)
	 (minx 0.0)
	 (maxx 1.0)
	 (extents-group #f)

	 (action-hbox (gtk-hbox-new #f 10))
	 (separator (gtk-hseparator-new))
	 (cancel-btn (gtk-button-new-with-label "Cancel"))
	 (export-btn (gtk-button-new-with-label "Export"))
	 )
    
    (gtk-window-set-title window "Export Data")
    (gtk-container-border-width window 0)
    (gtk-container-add window vbox)
    (gtk-widget-show vbox)

    (gtk-container-border-width hbox1 10)
    (gtk-box-pack-start vbox hbox1 #f #t 0)
    (gtk-widget-show hbox1)

    (gtk-box-pack-start hbox1 outf-label #f #t 0)
    (gtk-widget-show outf-label)

    (gtk-box-pack-start hbox1 filename-entry #f #t 0)
    (gtk-widget-show filename-entry)
    (gtk-entry-set-text filename-entry "gwexport.dat")

    (gtk-box-pack-start hbox1 browse-btn #f #t 0)
    (gtk-signal-connect browse-btn "clicked"
			(lambda ()
			  (with-selected-filename 
			   "Export to file"
			   (lambda (f)
			     (gtk-entry-set-text filename-entry f))
			   #:default (gtk-entry-get-text filename-entry))))
    (gtk-widget-show browse-btn)

    ; row of buttons for x-extents to export
    (gtk-container-border-width hbox2 10)
    (gtk-box-pack-start vbox hbox2 #f #t 0)
    (gtk-widget-show hbox2)

    (set! extents-group (add-radio-button 
		      hbox2 extents-group "All" #t
		      (lambda () 
			(display "setting all\n")
			(set! use-extents #f))))
    (set! extents-group (add-radio-button 
		      hbox2 extents-group "Visible" #f
		      (lambda ()
			(display "setting visible\n")
			(set! use-extents #t)
			(set! minx (wtable-start-xval))
			(set! maxx (wtable-end-xval)))))
    (set! extents-group (add-radio-button 
		      hbox2 extents-group "Between Cursors" #f
		      (lambda () 
			(display "setting tween-cursor\n")
			(set! use-extents #t)
			(set! minx (wtable-vcursor 0))
			(set! maxx (wtable-vcursor 1)))))

    ; row of action buttons
    (gtk-container-border-width action-hbox 10)
    (gtk-box-pack-start vbox action-hbox #f #t 0)
    (gtk-widget-show action-hbox)

    (gtk-signal-connect cancel-btn "clicked" 
			(lambda () 
			  (gtk-widget-destroy window)))
    (gtk-box-pack-start action-hbox cancel-btn #t #t 0)
    (gtk-widget-show cancel-btn)
    (gtk-tooltips-set-tip gwave-tooltips cancel-btn
			  "Cancel export and close window" "")

    (gtk-box-pack-start action-hbox export-btn #t #t 0)
    (gtk-signal-connect export-btn "clicked" 
			(lambda ()
			  (if (and use-extents (number? minx) (number? maxx))
			      (export-variables-to-file 
			       (gtk-entry-get-text filename-entry)
			       wvlist minx maxx)
			      (export-variables-to-file 
			       (gtk-entry-get-text filename-entry) wvlist))
			  (gtk-widget-destroy window)))
    (gtk-widget-show export-btn)
    (gtk-tooltips-set-tip gwave-tooltips export-btn
			  "Export data" "")
    
    (gtk-widget-set-flags export-btn '(can-default))
    (gtk-widget-grab-default export-btn)
    (gtk-widget-show window)
))


(define-public (popup-plot-dialog plist)
  (let* ((window (gtk-window-new 'toplevel))
	 (vbox (gtk-vbox-new #f 0))
	 (hbox1 (gtk-hbox-new #f 10))
	 (outf-label (gtk-label-new "Output File:"))
	 (filename-entry (gtk-entry-new))
	 (browse-btn (gtk-button-new-with-label "Browse"))

	 (hbox2 (gtk-hbox-new #f 10))
	 (tmpfcheck (gtk-check-button-new-with-label "Keep Tempfiles"))

	 (notebook (gtk-notebook-new))
	 (options-procedure #f)
	 (plot-procedure #f)
	 (plot-options '())

	 (action-hbox (gtk-hbox-new #f 10))
	 (separator (gtk-hseparator-new))
	 (cancel-btn (gtk-button-new-with-label "Cancel"))
	 (export-btn (gtk-button-new-with-label "Plot"))
	 (oproc-assoc '())
	 )
    
    (gtk-window-set-title window "Plot Data")
    (gtk-container-border-width window 0)
    (gtk-container-add window vbox)
    (gtk-widget-show vbox)

    (gtk-container-border-width hbox1 10)
    (gtk-box-pack-start vbox hbox1 #f #t 0)
    (gtk-widget-show hbox1)

    (gtk-box-pack-start hbox1 outf-label #f #t 0)
    (gtk-widget-show outf-label)

    (gtk-box-pack-start hbox1 filename-entry #f #t 0)
    (gtk-widget-show filename-entry)
    (gtk-entry-set-text filename-entry "gwplot.dat")

    (gtk-box-pack-start hbox1 browse-btn #f #t 0)
    (gtk-signal-connect browse-btn "clicked"
			(lambda ()
			  (with-selected-filename 
			   "Plot to file"
			   (lambda (f)
			     (gtk-entry-set-text filename-entry f))
			   #:default (gtk-entry-get-text filename-entry))))
    (gtk-widget-show browse-btn)

    ; notebook with entry for each supported plot filter
    ; containing that filter's various options
    (gtk-notebook-set-tab-pos notebook 'top)
    (gtk-box-pack-start vbox notebook #t #t 0)

;    (dump-plotf-list)
    (for-each (lambda (exptype)
		(let* ((panelproc ( (cadr exptype) ))
		       (panel (car panelproc))
		       (optproc (cadr panelproc))
		       (plotproc (caddr exptype))
		       (label (gtk-label-new (car exptype))))
		  (gtk-notebook-append-page notebook panel label)
		  
		  ; TODO extract these based on the current
		  ; notebook tab when go button clicked
		  (set! oproc-assoc (assoc-set! oproc-assoc plotproc optproc))

		  (set! options-procedure optproc)
		  (set! plot-procedure plotproc)))
	      plot-list)
 ;    (format #t "oproc-assoc: ~s\n" oproc-assoc)
    (gtk-widget-show notebook)

    ; general options
    (gtk-box-pack-start hbox2 tmpfcheck #f #t 0)
    (gtk-widget-show tmpfcheck)
    (gtk-widget-show hbox2)
    (gtk-box-pack-start vbox hbox2 #t #t 0)

    ; row of action buttons
    (gtk-container-border-width action-hbox 10)
    (gtk-box-pack-start vbox action-hbox #f #t 0)
    (gtk-widget-show action-hbox)

    (gtk-signal-connect cancel-btn "clicked" 
			(lambda () 
			  (gtk-widget-destroy window)))
    (gtk-box-pack-start action-hbox cancel-btn #t #t 0)
    (gtk-tooltips-set-tip gwave-tooltips cancel-btn
			  "Cancel export and close window" "")
    (gtk-widget-show cancel-btn)

    (gtk-box-pack-start action-hbox export-btn #t #t 0)
    (gtk-tooltips-set-tip gwave-tooltips export-btn "Plot data" "")
    (gtk-signal-connect export-btn "clicked" 
			(lambda ()
			  (let* ((n (gtk-notebook-get-current-page notebook))
				 (pp (caddr (list-ref plot-list n)))
				 (op (assoc-ref oproc-assoc pp))
				 (optlist (if (procedure?  op) (op) (list))))
;			    (format #t "plot filter ~d ~s ~s\n" n op pp)
;			    (format #t "opts: ~s\n" optlist)
			    (if (procedure? pp)
				(pp
				 (gtk-entry-get-text filename-entry) 
				 plist optlist
				(gtk-toggle-button-active tmpfcheck))))
			  (gtk-widget-destroy window)))
    (gtk-widget-show export-btn)
    
    (gtk-widget-set-flags export-btn '(can-default))
    (gtk-widget-grab-default export-btn)
    (gtk-widget-show window)
))

; run a command in a subprocess, redirecting its output to a named file.
(define-public (subprocess-to-file f cmd arglist)
  (let ((port (open f (logior O_WRONLY O_CREAT O_TRUNC) #o0777))
	(null (open "/dev/null" O_RDONLY 0)))
    ;(format #t "subprocess-to-file ~a ~s\n" cmd arglist)
    (flush-all-ports)
    ; TODO: stat cmd to make sure its executable.
    (let ((p (primitive-fork)))
      (cond ((< p 0)
	     ; error
	     (error "fork"))
	    ((eq? 0 p)
	     ; child
	     (redirect-port port (current-output-port))
	     (redirect-port null (current-input-port))
	     (close-all-ports-except (current-input-port) 
				     (current-output-port) 
				     (current-error-port))
	     (false-if-exception
	      (begin
		(apply execlp cmd arglist)
		(primitive-exit 127)))
	     )
	    (else 
	     ; parent
	     (close port)
	     
	     (format #t "child process ~d started for ~a ~s\n" p cmd arglist)
	     (reap-child)
	     )))))

(define (reap-child)
  (let* ((w (catch 'system-error
	    (lambda () (waitpid 0 WNOHANG))
	    (lambda (func . stuff) (cons 0 #f))))
	 (pid (car w))
	 (st (cdr w)))
    (if (not (eq? 0 pid))
	(begin
	  (format #t "process ~d" pid)
	  (if (status:exit-val st)
	      (format #t " exited (~d)" (status:exit-val st)))
	  (if (status:term-sig st)
	      (format #t " terminated on signal ~d" (status:term-sig st)))
	  (if (status:stop-sig st)
	      (format #t " stopped on signal~d" (status:exit-sig st)))
	  (display "\n"))
         ;(display "no child\n")
	)))

(dbprint "setting SIGCHLD handler\n")
;(display (sigaction SIGCHLD (lambda (s) (reap-child)))) (newline)
;(display (sigaction SIGCHLD)) (newline)
(dbprint "export.scm done")

