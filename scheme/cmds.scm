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

(define-public (x-zoom-full!)
  (x-zoom! (wtable-min-xval) (wtable-max-xval)))

(define-public (x-zoom-area!)
  (select-range-x 
   (lambda (wp x1 x2)
;     (display "in zoom-area callback ")
;     (display wp) (display " ")
;     (display x1) (display " ")
;     (display x2) (newline)
     (x-zoom! (wavepanel-x2val wp x1) (wavepanel-x2val wp x2)))))

; zoom relative to current position.
; if zoom factor > 1,  zooms in
; if zoom factor < 1,  zooms out
; TODO: make this do the right thing when the X scale is logarithmic.
(define-public (x-zoom-relative! zf)
  (let* ((sx (wtable-start-xval))
	 (ex   (wtable-end-xval))
	 (center ( / (+ sx ex) 2))
	 (width (- ex sx)))
    (x-zoom! ( - center (/ width (* zf 2)))
	   ( + center (/ width (* zf 2))))))

; zoom X so that edges of displayed are where the vertical cursors are now.
; If both vertical bar cursors aren't displayed, do nothing.
;
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

;
; create and show a top-level window 
; with the "about" information
;
(define-public (show-about-window!)
  (let* ((window (gtk-widget-new 'GtkWindow
				 :type         'toplevel
				 :title        "About Gwave"
				 :GtkContainer::border_width 10))
	 (vbox (gtk-vbox-new #f 10)))
    (gtk-widget-show vbox)
    (gtk-container-add window vbox)
    (let ((llab (gtk-label-new 
		 (string-append "gwave version " gwave-version-string))))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))
    (let ((llab (gtk-label-new "by Steve Tell")))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))
    (let ((llab (gtk-label-new "tell@cs.unc.edu")))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))
    (make-button vbox "Close" (lambda () (gtk-widget-destroy window)))
    (gtk-widget-show window)))

;
; Put up a file-selection dialog with title S.
; When file seleted, run procedure P, passing it the name of the file.
; Optionaly, a default suggested filename can be specified
;
(define*-public (with-selected-filename s p #&key default)
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

    (if (bound? default)
	(gtk-file-selection-set-filename window default))
    (gtk-file-selection-hide-fileop-buttons window)
    (gtk-widget-show window)
))

; Add variable to wavepanel, and then do setup of its color, style, etc.
; Mainly for use from scripts that restore a saved configuration.
; TODO: don't add the variable if already present in the specified panel.
(define-public (wavepanel-add-var-setup df wp signame color)
  (if df
      (let ((var (wavefile-variable df signame)))
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

(define-public (nth-wavepanel n)
  (list-ref (wtable-wavepanels) n))

; return GWDataFile object for named file, or #f it there is no such file
(define-public (find-wavefile name)
  (call-with-current-continuation
   (lambda (exit)
     (for-each (lambda (df)
		 (if (string=? name (wavefile-file-name df))
		     (exit df)))
              (wavefile-list))
     #f)))

; locate a already-loaded wavefile by name, and if that fails,
; try to load it.  If that fails too, return #f.
(define-public (find-or-load-wavefile name)
  (let* ((df (find-wavefile name)))
    (if (not df)
	(load-wavefile! name)
	df)))
;
; Write out a guile script that when executed by a future gwave,
; will restore the configuration of waves displayed from a particular datafile
(define-public (write-filerestore-script df fname)
  (let ((p (open fname (logior O_WRONLY O_CREAT) #o0777)))
    (with-output-to-port p 
      (lambda () 
	(write-script-header)
	(write-wfr-script df)))
    (close-port p)))

; Similar, but writes configuration-restoring script for all datafiles
(define-public (write-allrestore-script sname)
  (let ((p (open sname (logior O_WRONLY O_CREAT) #o0777)))
    (print "port=" p "\n")
    (with-output-to-port p 
      (lambda () 
	(write-script-header)
	(for-each (lambda (df) (write-wfr-script df))
		  (wavefile-list))))
    (close-port p)))

; write header part of configuration-restoring script, 
; specifying "/path/to/gwave -s" as its interpreter.
(define (write-script-header)
  (print "#!" gwave-bin-gwave-path " -s\n!#\n")
  (print "; gwave script\n")
  (print "(require-n-wavepanels " (length (wtable-wavepanels)) ")\n")
)

; write portion of script to restore waves for a single wavefile
(define (write-wfr-script df)
  (print "(let ((df (find-or-load-wavefile \"")
  (print (wavefile-file-name df) "\")))\n")
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
	       (print " (wavepanel-add-var-setup df (nth-wavepanel " n
		      ") \"" 
		      (visiblewave-varname vw) "\" " 
		      (visiblewave-color vw) " )\n")
	       ))
	 (wavepanel-visiblewaves (car panels)))
	(write-wfrp-lines df (cdr panels) (+ n 1)))))
