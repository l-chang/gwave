;
; module providing some simple gwave commands
;

(define-module (app gwave cmds)
  :use-module (gtk gtk)
  :use-module (ice-9 optargs)
)
(read-set! keywords 'prefix)

; print list
(define-public (print . l)
  (for-each (lambda (e) (display e (current-output-port))) l))

(define-public (append-hook! hook proc)
  "Add PROC to HOOK at the end of the list."
  (add-hook! hook proc #t))

; x-axis zoom in/zoom out zoom by this ammount
(define x-zoom-fraction 2)

;; Zoom the x axis to show the entire independent-variable
;; range used by all displayed waveforms.
(define-public (x-zoom-full!)
  (x-zoom! (wtable-min-xval) (wtable-max-xval)))

;; Prompt the user to select a range along the x axis with the mouse,
;; and then zoom in so that the selected range fills the entire displayed
;; X axis.
(define-public (x-zoom-area!)
  (select-range-x 
   (lambda (wp x1 x2)
;     (display "in x-zoom-area callback ")
;     (display wp) (display " ")
;     (display x1) (display " ")
;     (display x2) (newline)
     (x-zoom! (wavepanel-x2val wp x1) (wavepanel-x2val wp x2)))))

;; Prompt the user to select with the mouse a range along the Y axis of
;; a particular wavepanel, and then vertically zoom that wavepanel
;; so that the selected range fills its entire displayed Y axis.
(define-public (y-zoom-range!)
  (select-range-y 
   (lambda (wp y1 y2)
;     (print "y-zoom-range callback(" wp ") ")
;     (print y1 " -> " (wavepanel-y2val wp y1) ", ")
;     (print y2 " -> " (wavepanel-y2val wp y2) "\n")
     (wavepanel-y-zoom! wp (wavepanel-y2val wp y1) (wavepanel-y2val wp y2))
)))

 
;; Restore a WavePanel to display the full range of Y values,
;; and to automaticly rescale as VisibleWaves are added and deleted.
(define-public (y-zoom-fullauto! wp) (wavepanel-y-zoom! wp #f #f))

;; Prompt the user to select a rectangular region of a WavePanel, and
;; then zoom in both X and Y so that the selected area fills the whole
;; window.
(define-public (xy-zoom-area!)
  (select-range-xy 
   (lambda (wp x1 x2 y1 y2)
;     (display "in xy-zoom-area callback ")
;     (display wp) (display " ")
;     (display x1) (display " ")
;     (display x2) (display " ")
;     (display y1) (display " ")
;     (display y2) (newline)
     (x-zoom! (wavepanel-x2val wp x1) (wavepanel-x2val wp x2))
     (wavepanel-y-zoom! wp (wavepanel-y2val wp y1) (wavepanel-y2val wp y2))
)))

(define (pow base power)
    (exp (* power (log base))))

;; zoom the display's X axis relative to current configuration.
;; if the zoom factor is greater than 1, we zoom in.
;; if the zoom factor is less than 1, we zoom out.
(define-public (x-zoom-relative! zf)
  (let ((sx (wtable-start-xval))
 	(ex   (wtable-end-xval)))
    (if (not (wtable-xlogscale?))
	(let ((center (/ (+ sx ex) 2))
 	      (width (- ex sx)))
 	  (x-zoom! (- center (/ width (* zf 2)))
 		   (+ center (/ width (* zf 2)))))
 	(let ((center (sqrt (* ex sx)))
 	      (width (/ ex sx)))
 	  (x-zoom! (/ center (pow width (/ 0.5 zf)))
 		   (* center (pow width (/ 0.5 zf))))))))

;; zoom X so that edges of displayed are where the vertical cursors are now.
;; If both vertical bar cursors aren't displayed, do nothing.
;;-
; FIXME:tell:
;  pop message somewhere if both cursors not displayed
;  zoom so that cursors are just visible at edges, say 5% in from edge.
(define-public (x-zoom-cursors!)
  (let ((c0 (wtable-vcursor 0))
	(c1 (wtable-vcursor 1)))
    (if (and c0 c1)
	(x-zoom! c0 c1))))

;
; Implement a simple notion of WavePanel "type" that changes
; several of the lower-level options together.   Earlier versions
; had this in C.
;

; instead of multiple lists, these should be a real data structure of some kind
(define-public wavepanel-type-names    (list "Std"	"Jge"))
(define-public wavepanel-num-types (length wavepanel-type-names))
(define            panel-type-heights  (list 100 	25))
(define            panel-type-showlabs (list #t  	#f))

(define-public (set-wavepanel-type! wp type)
  (set-object-property! wp 'wp-type type)
  (set-wavepanel-minheight! wp (list-ref panel-type-heights type))
  (set-wavepanel-ylabels-visible! wp (list-ref panel-type-showlabs type)))

(define-public (wavepanel-type wp) (object-property wp 'wp-type))

; wrapper around wtable-insert-panel that pays attention to this type business
(define-public (wtable-insert-typed-panel! wp type)
  (wtable-insert-panel! wp
		       (list-ref panel-type-heights type)
		       (list-ref panel-type-showlabs type)))

; Add the panel-type property to a new wavepanel, so the context-sensitive
; menu works properly.
; For the moment, the standard GUI stuff only calls 
; wtable-insert-typed-panel! with the default type, so this comes out OK.
; Really need to pass the type through from wtable-insert-typed-panel
; somehow.  Or else, change the interface.
(add-hook! new-wavepanel-hook 
	   (lambda (wp)
	     (dbprint "in cmds.scm new-wavepanel-hook " wp "\n")
	     (set-object-property! wp 'wp-type default-wavepanel-type)))

; GTK+ helper: make a simple button with a textual label
(define (make-button parent txt func) 
  (let* ((btn (gtk-button-new-with-label txt)))
    (gtk-container-add parent btn)
    (gtk-widget-show btn)
    (if func (gtk-signal-connect btn "clicked" func))
    btn))

;; Create and show a top-level window with the "about" information
(define-public (show-about-window!)
  (let* ((window (gtk-widget-new 'GtkWindow
				 :type         'toplevel
				 :title        "About Gwave"
				 :GtkContainer::border_width 10))
	 (vbox (gtk-vbox-new #f 10)))
    (gtk-widget-show vbox)
    (gtk-container-add window vbox)
    (let ((llab (gtk-label-new 
		 (string-append "Gwave version " gwave-version-string))))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))
    (let ((llab (gtk-label-new "Copyright 2001-2003 Steve Tell")))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))

    (let ((llab (gtk-label-new "steve@telltronics.org")))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))

    (let ((llab (gtk-label-new "Gwave comes with ABSOLUTELY NO WARRANTY.")))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))

    (let ((llab (gtk-label-new "This is Free Software, and you are welcome to distribute")))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))
    (let ((llab (gtk-label-new "it under the terms of the GNU General Public License.")))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))

    (make-button vbox "Close" (lambda () (gtk-widget-destroy window)))
    (gtk-widget-show window)))

;; Pop up a dialog box to enter new axis limits (zoom setting) for a wavepanel.
(define-public (show-zoom-dialog! wp)
  (let* ((window (gtk-widget-new 'GtkWindow
				 :type         'toplevel
				 :title        "Gwave axis settings"
				 :GtkContainer::border_width 5))
	 (vbox (gtk-vbox-new #f 5))
	 (hbox (gtk-hbox-new #f 5))
	 (frame_x (gtk-frame-new "Global X Axis"))
	 (table_x (gtk-table-new 3 3 #f))
	 (start_x_entry (gtk-entry-new))
	 (end_x_entry (gtk-entry-new))
	 (frame_y (gtk-frame-new "Panel Y Axis"))
	 (table_y (gtk-table-new 3 4 #f))
	 (start_y_entry (gtk-entry-new))
	 (end_y_entry (gtk-entry-new))
	 (man_y_button (gtk-toggle-button-new-with-label "Auto Full-Scale"))
	 (max-rect (wavepanel-max-rect wp))
	 (disp-rect (wavepanel-disp-rect wp))
	 )
    (gtk-table-set-row-spacings table_x 3)
    (gtk-table-set-col-spacings table_x 3)
    (gtk-table-set-row-spacings table_y 3)
    (gtk-table-set-col-spacings table_y 3)
    (gtk-container-add vbox frame_x)
    (gtk-widget-show frame_x)
    (gtk-container-add frame_x table_x)
    (gtk-widget-show table_x)
    (let ((lab (gtk-label-new "min,max:")))
      (gtk-table-attach table_x lab 0 1 0 1)
      (gtk-widget-show lab))
    (let ((lab (gtk-label-new (number->spice (car max-rect))) ))
      (gtk-table-attach table_x lab 1 2 0 1)
      (gtk-widget-show lab))
    (let ((lab (gtk-label-new (number->spice (caddr max-rect))) ))
      (gtk-table-attach table_x lab 2 3 0 1)
      (gtk-widget-show lab))

    (let ((lab (gtk-label-new "Current:")))
      (gtk-table-attach table_x lab 0 1 1 2)
      (gtk-widget-show lab))
    (let ((lab (gtk-label-new (number->spice (car disp-rect))) ))
      (gtk-table-attach table_x lab 1 2 1 2)
      (gtk-widget-show lab))
    (let ((lab (gtk-label-new (number->spice (caddr disp-rect))) ))
      (gtk-table-attach table_x lab 2 3 1 2)
      (gtk-widget-show lab))

    (let ((lab (gtk-label-new "New:")))
      (gtk-table-attach table_x lab 0 1 2 3)
      (gtk-widget-show lab))

    (gtk-table-attach table_x start_x_entry 1 2 2 3)
    (gtk-entry-set-text start_x_entry (number->spice (car disp-rect)))
    (gtk-widget-show start_x_entry)
    (gtk-table-attach table_x end_x_entry 2 3 2 3)
    (gtk-entry-set-text end_x_entry (number->spice (caddr disp-rect)))
    (gtk-widget-show end_x_entry)

	;  second part: Y stuff
   (gtk-container-add vbox frame_y)
    (gtk-widget-show frame_y)
    (gtk-container-add frame_y table_y)
    (gtk-widget-show table_y)
    (let ((lab (gtk-label-new "min,max:")))
      (gtk-table-attach table_y lab 0 1 0 1)
      (gtk-widget-show lab))
    (let ((lab (gtk-label-new (number->spice (cadr max-rect))) ))
      (gtk-table-attach table_y lab 1 2 0 1)
      (gtk-widget-show lab))
    (let ((lab (gtk-label-new (number->spice (cadddr max-rect))) ))
      (gtk-table-attach table_y lab 2 3 0 1)
      (gtk-widget-show lab))

    (let ((lab (gtk-label-new "Current:")))
      (gtk-table-attach table_y lab 0 1 1 2)
      (gtk-widget-show lab))
    (let ((lab (gtk-label-new (number->spice (cadr disp-rect))) ))
      (gtk-table-attach table_y lab 1 2 1 2)
      (gtk-widget-show lab))
    (let ((lab (gtk-label-new (number->spice (cadddr disp-rect))) ))
      (gtk-table-attach table_y lab 2 3 1 2)
      (gtk-widget-show lab))

    (if (wavepanel-y-manual? wp)
	(gtk-toggle-button-set-state man_y_button #f)
	(begin
	  (gtk-toggle-button-set-state man_y_button #t)
	  (gtk-widget-set-sensitive start_y_entry #f)
	  (gtk-widget-set-sensitive end_y_entry #f)))
    (gtk-signal-connect man_y_button "toggled" (lambda ()
			(if (gtk-toggle-button-active man_y_button)
			    (begin 
			      (gtk-widget-set-sensitive start_y_entry #f)
			      (gtk-widget-set-sensitive end_y_entry #f))
			    (begin
			      (gtk-widget-set-sensitive start_y_entry #t)
			      (gtk-widget-set-sensitive end_y_entry #t)))))
    (gtk-table-attach table_y man_y_button 0 1 2 3)
    (gtk-widget-show man_y_button)

    (let ((lab (gtk-label-new "New:")))
      (gtk-table-attach table_y lab 0 1 3 4)
      (gtk-widget-show lab))
    (gtk-table-attach table_y start_y_entry 1 2 3 4)
    (gtk-entry-set-text start_y_entry (number->spice (cadr disp-rect)))
    (gtk-widget-show start_y_entry)
    (gtk-table-attach table_y end_y_entry 2 3 3 4)
    (gtk-entry-set-text end_y_entry (number->spice (cadddr disp-rect)))
    (gtk-widget-show end_y_entry)

 ; 3rd part: button row
    (make-button hbox "OK"
	(lambda () 
	  (let ((n_sx (spice->number (gtk-entry-get-text start_x_entry)))
		(n_ex (spice->number (gtk-entry-get-text end_x_entry)))
		(n_sy (spice->number (gtk-entry-get-text start_y_entry)))
		(n_ey (spice->number (gtk-entry-get-text end_y_entry)))
		)
	    (x-zoom! n_sx n_ex)
	    (if (gtk-toggle-button-active man_y_button)
		(wavepanel-y-zoom! wp #f #f)
		(wavepanel-y-zoom! wp n_sy n_ey))
	    (gtk-widget-destroy window))))
    (make-button hbox "Cancel" (lambda () (gtk-widget-destroy window)))
    
    (gtk-widget-show hbox)
    (gtk-container-add vbox hbox)

    (gtk-widget-show vbox)
    (gtk-container-add window vbox)
    (gtk-widget-show window)
))

; Pop up a file-selection dialog with title S.
; When file seleted, run procedure P, passing it the name of the file.
; Optionaly, a default suggested filename can be specified using
; keyword #:default.
(define*-public (with-selected-filename s p #&key (default #f))
  (let* ((window (gtk-file-selection-new s))
         (button #f))
    (gtk-signal-connect
     (gtk-file-selection-ok-button window)
     "clicked" (lambda () 
		 (p (gtk-file-selection-get-filename window))
		 (gtk-widget-destroy window)
		 ))
			  
    (gtk-signal-connect 
     (gtk-file-selection-cancel-button window)
     "clicked" (lambda () (gtk-widget-destroy window)))

    (if (string? default)
	(gtk-file-selection-set-filename window default))
    (gtk-file-selection-hide-fileop-buttons window)
    (gtk-widget-show window)
))


;; Call set-visiblewave-measure! on all visiblewaves
;; to set the function for measurement number MNO to MFUNC.
(define-public (set-all-measurements! mno mfunc)
  (for-each (lambda (wp)
	      (for-each (lambda (vw)
			  (set-visiblewave-measure! vw mno mfunc))
			(wavepanel-visiblewaves wp)))
	    (wtable-wavepanels)))



; Add variable to wavepanel, and then do setup of its color, style, etc.
; Mainly for use from scripts that restore a saved configuration.
; TODO: don't add the variable if already present in the specified panel.
(define*-public (wavepanel-add-var-setup df wp signame color #:key (sweep 0))
  (if df
      (let ((var (wavefile-variable df signame sweep)))
	(if var
	    (let ((vw (wavepanel-add-variable! wp var)))
	      (if vw
		  (set-visiblewave-color! vw color)))))))


(define-public (require-n-wavepanels rn)
    (let ((hn (length (wtable-wavepanels))))
;      (if (< hn rn)
;	  (begin
;	    (print "need " (- rn hn) " more wavepanels\n")))
      (do ((i hn
	      (+ i 1)))
	  ((not (< i rn)))
	(wtable-insert-typed-panel! #f default-wavepanel-type))
      ))

(define-public (num-wavepanels)
  (length (wtable-wavepanels)))
  
(define-public (nth-wavepanel n)
  (list-ref (wtable-wavepanels) n))

(define-public (unselect-all-wavepanels!)
  (for-each (lambda (wp)
	      (set-wavepanel-selected! wp #f))
	    (wtable-wavepanels)))

;; Given a filename, return the GWDataFile object associated with
;; the data loaded from that file, or #f it there is no such file loaded.
(define-public (find-wavefile name)
  (call-with-current-continuation
   (lambda (exit)
     (for-each (lambda (df)
		 (if (string=? name (wavefile-file-name df))
		     (exit df)))
              (wavefile-list))
     #f)))

;; locate a already-loaded wavefile by name, and if that fails,
;; try to load it.  If that fails too, return #f.
(define-public (find-or-load-wavefile name)
  (let* ((df (find-wavefile name)))
    (if (not df)
	(load-wavefile! name)
	df)))

;; Write out a guile script that when executed by a future gwave,
;; will restore the configuration of waves displayed from 
;; one particular datafile.
(define-public (write-filerestore-script df fname)
  (let ((p (open fname (logior O_WRONLY O_CREAT O_TRUNC) #o0777)))
    (with-output-to-port p 
      (lambda () 
	(write-script-header)
	(write-wfr-script df #t)
	(write-script-trailer)
	))
    (close-port p)))

;; Write out a guile script that when executed by a future gwave,
;; will restore the configuration of all currently-displayed waves.
(define-public (write-allrestore-script sname)
  (let ((p (open sname (logior O_WRONLY O_CREAT O_TRUNC) #o0777))
	(mfs (eqv? 1 (length (wavefile-list)) )))
    (with-output-to-port p 
      (lambda () 
	(write-script-header)
	(for-each (lambda (df) (write-wfr-script df mfs))
		  (wavefile-list))
	(write-script-trailer)
	))
    (close-port p)))

; write header part of configuration-restoring script, 
; specifying "/path/to/gwave -s" as its interpreter.
(define (write-script-header)
  (print "#!" gwave-bin-gwave-path " -s\n!#\n")
  (print "; gwave script\n")
  (print "(require-n-wavepanels " (length (wtable-wavepanels)) ")\n")
  (print "(set! default-measure1-function " default-measure1-function ")\n")
)

; write trailer part of config-restore script, which restores
; panel and global display parameters, and global preferences.
; BUG: tooltips, wavepanel-tpe, and X-logscale are restored,
; but the radio-buttons in the Options menu are not affected.
;
(define (write-script-trailer)
  (print "(x-zoom! " (wtable-start-xval) " " (wtable-end-xval) ")\n")
  (print "(wtable-set-xlogscale! "(wtable-xlogscale?) ")\n")
  (print "(set! default-wavepanel-type " default-wavepanel-type")\n")
  (if (wtable-vcursor 0)
      (print "(set-wtable-vcursor! 0 " (wtable-vcursor 0) ")\n"))
  (if (wtable-vcursor 1)
      (print "(set-wtable-vcursor! 1 " (wtable-vcursor 1) ")\n"))

  (if (gtk-tooltips-enabled? gwave-tooltips)
      (print "(gtk-tooltips-enable gwave-tooltips)\n")
      (print "(gtk-tooltips-disable gwave-tooltips)\n"))
  (do ((i 0
	  (+ i 1))) 
      ((not (< i (num-wavepanels))))
    (let ((wp (nth-wavepanel i)))
      (print "(let ((wp (nth-wavepanel "i")))\n")
      (print " (set-wavepanel-ylogscale! wp "(wavepanel-ylogscale? wp) ")\n")
      (print " (set-wavepanel-type! wp " (wavepanel-type wp) ")\n")
      (if (wavepanel-y-manual? wp)
	  (let ((dr (wavepanel-disp-rect wp)))
	    (print " (wavepanel-y-zoom! wp " (cadr dr) " " (cadddr dr) ")\n")))
      (print ")\n")
      ))
)

; write portion of script to restore waves for a single wavefile
; If "multi" is #t, multiple file-restoration sections will be written
; to this script.  In this case, we don't provide for the "apply script
; to (already loaded) file" function.
(define (write-wfr-script df multi)
  (if multi
      (begin
	(print "(let ((df (if script-target-datafile\n"
	       "           script-target-datafile\n"
	       "           (find-or-load-wavefile \""
	       (wavefile-file-name df)  "\"))))\n"))
      (print "(let ((df (find-or-load-wavefile \""
	     (wavefile-file-name df) "\")))\n"))
  (let ((panels (wtable-wavepanels)))
    (write-wfrp-lines df panels 0))
  (print ")\n")
  )

; recursive part of writing script for single wavefile.
(define (write-wfrp-lines df panels n)
  (if (not (null? panels))
      (begin
	(for-each 
	 (lambda (vw)
	   (if (eq? df (visiblewave-file vw))
	       (begin
		 (print " (wavepanel-add-var-setup df (nth-wavepanel " n
			") \"" 
			(visiblewave-varname vw) "\" " 
			(visiblewave-color vw) )
		 (if (not (eq? 0 (variable-sweepindex vw)))
		     (print " #:sweep " (variable-sweepindex vw)))
		 (print ")\n"))
	       ))
	 (wavepanel-visiblewaves (car panels)))
	(write-wfrp-lines df (cdr panels) (+ n 1)))))

;; execute a guile script, ignoring any errors.
(define-public (execute-script fname)
  (false-if-exception (load fname))
)

; global to pass target datafile smob to scripts executed
; by apply-script-to-file.
(define-public script-target-datafile #f)

;; execute a a guile script that was saved by a
;; call to write-filerestore-script, 
;; passing it the name of an alternate data file to load in place of the
;; file specified in the script.
(define-public (apply-script-to-file fname dfile)
  (set! script-target-datafile dfile)
  (false-if-exception (load fname))
  (set! script-target-datafile #f)
)
