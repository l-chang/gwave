;
; module providing gwave support for plot/export using GNU graph
; as the backend.
;

(define-module (app gwave export-gnugraph)
  :use-module (gtk gtk)
  :use-module (ice-9 optargs)
  :use-module (ice-9 format)
  :use-module (app gwave cmds)
  :use-module (app gwave export)
)
(read-set! keywords 'prefix)




; build and return an option-menu button
; this is an optionmenu that drops down when a button is pressed;
; we return the button.
; call proc with new value on menu selection.
(define (build-option-menu proc optlist)
  (let ((menu (gtk-menu-new))
	(group #f)
	(optionmenu (gtk-option-menu-new))
	(vbox (gtk-vbox-new #f 0))
	(fixed (gtk-fixed-new)))

    (define (add-optlist-to-menu menu proc optlist)
      (let* ((label (gtk-label-new ""))
	     (menuitem (gtk-radio-menu-item-new group))
	     (eventbox (gtk-event-box-new)))

        (if (not (null? optlist))
	    (begin
	      (gtk-label-set-text label (car (car optlist)))
	      (gtk-container-add menuitem label)
	      (gtk-widget-show label)
	      (set! group menuitem)
	      (gtk-menu-append menu menuitem)
	      (gtk-widget-show menuitem)
	      (gtk-signal-connect menuitem "toggled"
				  (lambda () 
					; (if (menuitem is active) ;how to find this?
				    (begin
				      (proc (cdr (car optlist))))))
	      (add-optlist-to-menu menu proc (cdr optlist))))
	))

    (add-optlist-to-menu menu proc optlist)
    (gtk-widget-show menu)
    (gtk-option-menu-set-menu optionmenu menu)
;    (gtk-option-menu-set-history optionmenu )
    (gtk-box-pack-start vbox optionmenu #f #f 0)
    (gtk-widget-show optionmenu)

    (gtk-widget-set-usize fixed 30 10)
    (gtk-signal-connect fixed "button_press_event"
                      (lambda (e)
                        (if (= (gdk-event-button e) 1)
                            (gtk-menu-popup menu #f #f
                                            (gdk-event-button e)
                                            (gdk-event-time e)))))
    (gtk-box-pack-start vbox fixed #t #t 0)
    (gtk-widget-show fixed)
    (gtk-widget-show vbox)
    vbox))


; Build a sub-dialog for gnu GRAPH plot options and attach it
; to a notebook.
; Returns a procedure, which when called, will return
; a plot options list.  The plot options list will be
; passed unchanged to the export procedure.
(define-public (add-gnugraph-panel notebook)
  (let* ((frame (gtk-frame-new "GNU Graph"))
	 (label (gtk-label-new "GNU Graph"))
	 (hbox (gtk-hbox-new #f 5))
	 (vbox (gtk-vbox-new #f 5))
	 (opt-format "ps")

	 (format-optmenu (build-option-menu 
			  (lambda (f) (set! opt-format f))
			  (list '("Postscript" . "ps")
				'("Portable Network Graphics" . "png")
				'("Portable Anymap" . "pnm")
				'("Scalable Vector Graphics" . "svg")
				'("Fig" . "fig")
				'("Gnu Graphics Metafile" . "meta")))))

	 (gtk-container-border-width frame 10)
	 (gtk-widget-set-usize frame 200 150)
	 (gtk-widget-show frame)
	 (gtk-container-add frame hbox)
	 (gtk-box-pack-start hbox format-optmenu #f #f 0)
	 (gtk-widget-show hbox)

; TODO: add at least these
;    (if color
;	(append! args '("-C")))
;    (if landscape
;	(append! args '("--rotation" "90")))

	 (gtk-notebook-append-page notebook frame label)
    (lambda ()
      (list "-T" opt-format))
))


; export a wavepanel's data in the format needed by gnu graph's 
; "a" input format.
(define (export-wavepanel-to-ggfile f wp)
  (let ((p (open f (logior O_WRONLY O_CREAT O_TRUNC) #o0777))
	(minx (wtable-start-xval))
	(maxx (wtable-end-xval)))
    (for-each (lambda (vw)
		(export-variables (cons vw '()) p minx maxx)
		(display "\n" p))
	      (wavepanel-visiblewaves wp))
    (close-port p)
  ))

; export-wavepanels-gnugraph - 
;
; generate hardcopy or documentary representation of the displayed
; waveforms on one or more wavepanels, using gnu graph as
; the formatting backend.

(define-public (export-wavepanels-gnugraph fname panellist options)
  (let* ((args (append (list "graph") options
		       (list "--input-format" "a"
			     "--width-of-plot" "0.9")))
	 (ngraphs (length panellist))
	 (idx 0))

    (append! args (list "--height-of-plot" 
		    (format #f "~f" (- (/ 0.9 ngraphs) 0.05))))
    (append! args '("--toggle-round-to-next-tick" "X"
		    "--font-size" "0.03"
		    "--grid-style" "3"))
    (if (wtable-xlogscale?)
	(append! args '("-l" "X")))
    (for-each (lambda (wp)
		 (if (> idx 0)
		       (set! args (append args (list "--reposition" "0"
						     (format #f "~f" (* idx (/ 0.9 ngraphs)))
						     "1"))))
		 (if (wavepanel-ylogscale? wp)
		     (append! args '("-l" "Y")))
		 (let ((fname (format #f "/tmp/gwplot.~s" idx)))
		   (export-wavepanel-to-ggfile fname wp)
		   (append! args (list fname)))
		 (if (wavepanel-ylogscale? wp)
		     (append! args '("-l" "Y")))
		 		 	
		 (set! idx (+ 1 idx))
		 )
	      panellist)
    ; finally we have the arglist
    (print "running graph"  args)
    (subprocess-to-file fname "graph" args)
))
