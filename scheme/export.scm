;
; module providing gwave commands and dialogs for exporting data
;

(define-module (app gwave export)
  :use-module (gtk gtk)
  :use-module (ice-9 optargs)
  :use-module (app gwave cmds)
)
(read-set! keywords 'prefix)


(define (export-variables-to-file f vwlist . ext)
 (let ((p (open f (logior O_WRONLY O_CREAT O_TRUNC) #o0777)))
   (print "ext is " ext "\n");
   (if (null? ext)
	  (export-variables vwlist p)
	  (export-variables vwlist p (car ext) (cadr ext)))
   (close-port p)
   ))

;
; create a gtk-radio-button that calls proc when selected,
; add it to the parent widget, and return it.
; Caller must still handle threading up the groups.
; TODO: write a function that takes a list of lists of labels & procs,
; and creates the whole set of radio-buttons.
;
(define-public (add-radio-button parent group label active proc)
  (let ((item (if label
		  (gtk-radio-button-new-with-label group label)
		  (gtk-radio button-new group))))
    (gtk-widget-show item)
    (if proc
	(gtk-signal-connect item "clicked" proc))
;			    (lambda ()
;			      (if (gtk-toggle-button-active item)
;				  proc))))
    (if active
	(gtk-toggle-button-set-state item active))
    (gtk-container-add parent item) 
    item
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
