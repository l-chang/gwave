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
  :use-module (app gwave gtk-helpers)
)
(read-set! keywords 'prefix)
(debug-enable 'backtrace)
(debug-enable 'debug)
; Build a sub-dialog for gnu GRAPH plot options and attach it
; to a notebook.
; Returns a procedure, which when called, will return
; a plot options list.  The plot options list will be
; passed unchanged to the export procedure.
(define-public (add-gnugraph-panel notebook)
  (let* ((frame (gtk-frame-new "GNU Graph"))
	 (label (gtk-label-new "GNU Graph"))
;	 (hbox (gtk-hbox-new #f 5))
	 (vbox (gtk-vbox-new #f 5))
	 (opt-format "ps")
	 (format-optmenu (build-option-menu 
			  (lambda (f) (set! opt-format f))
			  (list '("Postscript" . "ps")
				'("Portable Network Graphics" . "png")
				'("Portable Anymap" . "pnm")
				'("Scalable Vector Graphics" . "svg")
				'("Fig" . "fig")
				'("Gnu Graphics Metafile" . "meta"))))

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

    (gtk-notebook-append-page notebook frame label)
    (lambda ()
      (format #t "opt-landscape=~s\n" opt-landscape)
      (append (list "-T" opt-format)
	      (if opt-landscape
		(list "--rotation" "90")
		'())
	      (if opt-color
		(list "-C")
		'())
	      ))
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
