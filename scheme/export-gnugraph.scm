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


;
; export a wavepanel's data in the format needed by gnu graph's "a"
; input format.
;
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
; generate hardcopy or documentation representation of the displayed
; waveforms on one or more wavepanels, using gnu graph as
; the formatting backend.

(define-public (export-wavepanels-gnugraph fname outformat color landscape panellist)
  (let* ((args (list "-T" outformat 
		     "--input-format" "a"
		     "--width-of-plot" "0.9"))
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
    (print "running" args)
))
