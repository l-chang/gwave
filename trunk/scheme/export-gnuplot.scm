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
			  (list 
				'("Postscript" . "postscript")
				'("Computer Graphics Metafile" . "cgm")
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
				'("Postscript EPS" . "postscript eps")
				'("Scalable Vector Graphics" . "svg")
;				'("X11" . "x11")
				)))

	 (landscape-hbox (gtk-hbox-new #f 0))
	 (opt-landscape #f)
; landscape option to gnuplot doesn't seem to do anything 
;	 (landscape-rbtns (build-radiobutton-box 
;			   landscape-hbox
;			   (lambda (v) (set! opt-landscape v))
;			   (list '("Portrait" . #f)
;				 '("Landscape" . #t))))
	 (opt-color #f)
	 (color-rbtns (build-radiobutton-box 
		       (gtk-hbox-new #f 0)
		       (lambda (v) (set! opt-color v))
		       (list '("Greyscale" . #f)
			     '("Color" . #t) )))

	 (multiplot-check (gtk-check-button-new-with-label "Multiplot"))
	 )
						  
    (gtk-container-border-width frame 10)
;    (gtk-widget-set-usize frame 200 150)
    (gtk-widget-show frame)
    (gtk-container-add frame vbox)
    (gtk-box-pack-start vbox format-optmenu #f #f 0)

;    (gtk-box-pack-start vbox landscape-rbtns #f #f 0)
;    (gtk-widget-show landscape-rbtns)
    (gtk-box-pack-start vbox color-rbtns #f #f 0)
    (gtk-widget-show color-rbtns)
    (gtk-box-pack-start vbox multiplot-check #f #f 0)
    (gtk-widget-show multiplot-check)
    (gtk-toggle-button-set-state multiplot-check #t)
    (gtk-widget-show vbox)

    ; return list containing top-level frame and procedure
    (list frame
	  ; options-procedure returns a list consisting of a fixed-length
	  ; set of booleans followed by any number of strings that become
	  ; the preamble for the gnuplot script
	  (lambda ()
	    (append (list 
		     (gtk-toggle-button-active multiplot-check)
		     (format #f "set terminal ~a~a" opt-format
				  (if opt-color " color" "")
;				  (if opt-landscape " landscape" "")
				  )))))
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
  (let* ((multiplot (car options))
	 (preamble (append (list-copy (cdr options)) ))
	 (shfile (format #f "~a.gnuplot" (filter-metachars fname)))
	 (tmpbase (filter-metachars fname))
	 (tmpfilelist (list shfile))
	 (npanels (length panellist))
	 (minx (wtable-start-xval))
	 (maxx (wtable-end-xval))
	 (pidx 0)
	 (widx 0))

    (with-output-to-file shfile
      (lambda ()
	(format #t "~a\n" (join "\n" preamble))
	(format #t "set output \"~a\"\n" fname)
	(if multiplot
	    (format #t "set multiplot\nset size 1,~f\n" (/ 1 npanels)))
		
	(if (wtable-xlogscale?)
	    (display "set logscale x"))
	(display "\n")
	(for-each 
	 (lambda (wp)
	   (if multiplot
	       (format #t "set origin 0,~f\n" (* (- (- npanels 1) pidx) (/ 1 npanels))))
	   (if (wavepanel-ylogscale? wp)
	       (display "set logscale y\n")
	       (display "set nologscale y\n"))
	   (let ((plotlines '()))
	     (for-each 
	      (lambda (vw)
		(let* ((f (format #f "~a.~s" tmpbase widx))
		       (p (open f (logior O_WRONLY O_CREAT O_TRUNC) #o0777)))
		  (export-variables (cons vw '()) p minx maxx)
		  (set! plotlines (append plotlines (list
			(format #f " \"~a\" using 1:2 title \"~a\" with lines"
				f  (visiblewave-varname vw) ))))
		  (set! tmpfilelist (cons f tmpfilelist))
		  (close-port p)
		  (set! widx (+ 1 widx))))
	      (wavepanel-visiblewaves wp))
	     (format #t "plot ~a\n\n" (join ", \\\n" plotlines))
	     )
	   (set! pidx (+ 1 pidx))
	   )
	 panellist)
    	(if (not keeptmp)
	    (format #t "! rm -f ~a\n" (join " " tmpfilelist)))
	))
;   (format #t "plot-gnuplot cmdfile=~s tmpfilelist=~s\n" shfile tmpfilelist)
    (subprocess-to-file #f "gnuplot" (list "gnuplot" shfile))
))

(register-plotfilter "GNUPlot" 
		     build-plot-panel plot-wavepanels)

