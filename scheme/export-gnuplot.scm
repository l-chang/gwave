;
; module providing gwave support for plot/export using GNUPlot
; as the backend.
;

(define-module (app gwave export-gnuplot)
  :use-module (gtk gtk)
  :use-module (ice-9 optargs)
  :use-module (ice-9 format)
  :use-module (app gwave cmds)
  :use-module (app gwave export)
  :use-module (app gwave utils)
  :use-module (app gwave gtk-helpers)
)
(read-set! keywords 'prefix)
(debug-enable 'backtrace)
(debug-enable 'debug)

; Build a sub-dialog for gnu GRAPH plot options
; Returns a list of two items:
; - A GtkWidget for the notebook pannel of the dialog.
; - A procedure, which when called, will return
; a plot options list.  The plot options list will be
; passed unchanged to the export procedure.
(define (build-plot-panel)
  (let* ((frame (gtk-frame-new "GNUPlot"))
	 (vbox (gtk-vbox-new #f 5))
	 (opt-format "postscript")
	 (format-optmenu (build-option-menu 
			  (lambda (f) (set! opt-format f))
			  (list '("Computer Graphics Metafile" . "cgm")
				'("Drawing Exchange Format" . "dxf")
				'("Fig" . "fig")
				'("GPIC" . "gpic")
				'("HPGL" . "hpgl")
				'("LaTeX" . "latex")
				'("LaTeX Picture \Specials" . "pslatex")
				'("LaTeX Picture PSTricks" . "pstricks")
				'("LaTeX TPic \Specials" . "tpic")
				'("Maker Interchange Format" . "mif")
				'("Metafont" . "mf")
				'("Plain TeX Picture \Specials" . "pstex")
				'("Portable Anymap" . "pbm")
				'("Portable Network Graphics" . "png")
				'("Postscript" . "postscript")
				'("Scalable Vector Graphics" . "svg")
				'("X11" . "x11")
				)))

	 (landscape-hbox (gtk-hbox-new #f 0))
	 (opt-landscape #f)
	 (landscape-rbtns (build-radiobutton-box 
			   landscape-hbox
			   (lambda (v) (set! opt-landscape v))
			   (list '("Portrait" . #f)
				 '("Landscape" . #t))))
	 (opt-color #f)
	 (color-rbtns (build-radiobutton-box 
		       (gtk-hbox-new #f 0)
		       (lambda (v) (set! opt-color v))
		       (list '("Greyscale" . #f)
			     '("Color" . #t) )))
	 )
						  
    (gtk-container-border-width frame 10)
    (gtk-widget-set-usize frame 200 150)
    (gtk-widget-show frame)
    (gtk-container-add frame vbox)
    (gtk-box-pack-start vbox format-optmenu #f #f 0)
    (gtk-widget-show vbox)

    (gtk-box-pack-start vbox landscape-rbtns #f #f 0)
    (gtk-widget-show landscape-rbtns)
    (gtk-box-pack-start vbox color-rbtns #f #f 0)
    (gtk-widget-show color-rbtns)

    ; return list containing top-level frame and procedure
    (list frame
	  (lambda ()
;	    (format #t "opt-landscape=~s\n" opt-landscape)
	    (append (list (format #f "set terminal ~a~a" opt-format
				  (if opt-color " color" "")))
;		    (if opt-landscape (list "--rotation" "90") '())
		    )))
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

;
; generate hardcopy or documentary representation of the displayed
; waveforms on one or more wavepanels, using gnuplot as
; the formatting backend.
;
(define (plot-wavepanels fname panellist options keeptmp)
  (let* ((args (append (list-copy options) ))
	 (shfile (format #f "~a.gnuplot" (filter-metachars fname)))
	 (tmpbase (filter-metachars fname))
	 (tmpfilelist (list shfile))
	 (ngraphs (length panellist))
	 (minx (wtable-start-xval))
	 (maxx (wtable-end-xval))
	 (idx 0))

    (with-output-to-file shfile
      (lambda ()
	(display (join "\n" options))	(display "\n")
		
;    (set! args (append args (list "--height-of-plot" (format #f "~f" (- (/ 0.9 ngraphs) 0.05)))))
;    (set! args (append args '("--toggle-round-to-next-tick" "X" "--font-size" "0.03" "--grid-style" "3")))
	
	(format #t "set output \"~a\"\n" fname)
	(if (wtable-xlogscale?)
	    (display "set logscale x"))
	(for-each 
	 (lambda (wp)
;		(if (> idx 0)
;		    (set! args (append args (list "--reposition" "0"
;				(format #f "~f" (* idx (/ 0.9 ngraphs)))
;				 "1"))))
;		(if (wavepanel-ylogscale? wp)
;		    (set! args (append args '("-l" "Y"))))
	   (display "plot")
	   (for-each 
	    (lambda (vw)
	      (let* ((f (format #f "~a.~s" tmpbase idx))
		     (p (open f (logior O_WRONLY O_CREAT O_TRUNC) #o0777)))
		(export-variables (cons vw '()) p minx maxx)
		(format #t " \"~a\" using 1:2 with lines, \\\n" f)
		(set! tmpfilelist (cons fname tmpfilelist))
		(close-port p)
		(set! idx (+ 1 idx))))
		(wavepanel-visiblewaves wp))
	   (display "\n")
	   
;	 (if (wavepanel-ylogscale? wp)
;	     (set! args (append args '("-l" "Y"))))
	 		 	
	   )
	 panellist)
    
	(if (not keeptmp)
	    (format #t "! rm -f ~a\n" (join " " tmpfilelist)))
	))
    ; finally we have the arglist and tmpfilelist
    (format #t "plot-gnuplot cmdfile=~s tmpfilelist=~s\n"
	    shfile tmpfilelist)
; (format #t "running sh -C ~a\n" shfile)
    (subprocess-to-file fname "gnuplot" (list "gnuplot" shfile))
))

(register-plotfilter "GNUPlot" 
		     build-plot-panel plot-wavepanels)

