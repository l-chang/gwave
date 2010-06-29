;
; module providing GUI operations on VisibleWave objects
;   popup menu on right button in VisibleWave-button
;   multi-paneled dialog box accessible from the menu
;

(dbprint "visiblewave-ops.scm running\n")

(define-module (app gwave visiblewave-ops)
  :use-module (ice-9 format)
  :use-module (gnome-2)
  :use-module (gnome gtk)
  :use-module (gnome gtk gdk-event)
  :use-module (app gwave utils)
  :use-module (app gwave gtk-helpers)
  :use-module (app gwave std-menus)
  :use-module (app gwave export)
  :use-module (app gwave cmds)
  :use-module (app gwave globals)
)
(read-set! keywords 'prefix)

(debug-enable 'debug)
(read-enable 'positions)

; hook called when new VisibleWave is added.
(add-hook! 
 new-visiblewave-hook
 (lambda (vw)
   (dbprint "in exp new-visiblewave-hook " vw
	     "\n        file=" (visiblewave-file vw)
	     "\n     varname=" (visiblewave-varname vw)
	     "\n       panel=" (visiblewave-panel vw)
             "\n      button=" (visiblewave-button vw) "\n")

   (set-visiblewave-measure! vw 1 default-measure1-function)

   ; make this panel the only selected one when a wave is added to it
   (unselect-all-wavepanels!)
   (set-wavepanel-selected! (visiblewave-panel vw) #t)

   (gtk-signal-connect (visiblewave-button vw) "clicked" 
			(lambda (b)
			  ;(format #t "clicked ~s ~s\n" vw (gtk-toggle-button-get-active (visiblewave-button vw)))
			  ; TODO: redraw only the one panel affected
			  (wtable-redraw!) ))
   (gtk-signal-connect (visiblewave-button vw) "button-press-event" 
			(lambda (b event) 
;			  (display "press-signal") 
;			  (display event)
;			  (display (gdk-event:type event))
;			  (display "\n ")
;			  (display (gdk-event-button:button event))
;			  (newline)
			  (if (= (gdk-event-button:button event) 3)
			      (begin
				(gtk-menu-popup (make-vwb3-menu vw) 
						#f #f #f
						(gdk-event-button:button event)
						(gdk-event-button:time event))
				#t)
			      #f)))
			      
   (gtk-tooltips-set-tip gwave-tooltips (visiblewave-button vw)
			 (string-append 
			  (shorten-filename (wavefile-file-name (visiblewave-file vw)))
			  ":\n"
			  (visiblewave-varname vw)
			  "\nVisibleWave Button:\nClick button 1 to select wave.\nPress button 3 for options menu.") "")
))

(wavepanel-bind-mouse 1
 (lambda (wp event)
; (format #t "wavepanel ~s event=~s modifiers=~s\n" wp event 
;	 (gdk-event-button:modifiers event))
 
 (if (not (member 'shift-mask (gdk-event-button:modifiers event)))
     (unselect-all-wavepanels!))
 (set-wavepanel-selected! wp #t)
))

; create dynamic menu to be popped up with mousebutton 3 on the 
; visible-wave button.  Returns the menu.
(define (make-vwb3-menu vw)
  (let ((menu (gtk-menu-new)))
    (gtk-widget-show menu)
    (add-menuitem menu "Move to Top" 
		  (lambda () (visiblewave-on-top! vw)))
    (add-menuitem menu "Options..."
		  (lambda () (popup-vw-options vw)))
    (add-menuitem menu "Export..."
		  (lambda () (popup-export-dialog (cons vw '()))))
    (add-menuitem menu #f #f)
    (add-menuitem menu "Delete" 
		  (lambda () (visiblewave-delete! vw)))
    menu))

; build and return wave-color color menu.
; this is an optionmenu on a button; we return the button.
; call proc with new value on menu selection.
;
; TODO: redo using general GtkOptionMenu and a ListStore
; 	so we can hold labels or buttons whose color matches the color-numbers
;
(define (build-wavecolor-menu vw proc)
  (let ((combobox (gtk-combo-box-new-text)))

    (do ((i 0 (1+ i))
	 (j 0 (1+ j)))
	((= i 6))

;       (let* ((label (gtk-label-new 
; 		     (string-append "color " (number->string j))))
; 	(gtk-widget-set-name label 
; 			     (string-append "wavecolor" (number->string j)))
; 	(gtk-container-add menuitem eventbox)
; 	(gtk-container-add eventbox label))

      (append-text combobox (string-append "color " (number->string j))))

    (if (procedure? proc)
	(connect combobox 'changed 
		 (lambda (x)
		   (proc (get-active combobox)))))
    (set-active combobox  (visiblewave-color vw))
    (show combobox)
    combobox))

; build and attach frame for Style page of the VisibleWave options notebook
; style items include color, drawing algorithm, and drawing-alg parameters
; Returns a procedure that when will apply any changed style items
; to the VisibleWave.
(define (add-vw-opts-style-frame notebook vw)
  (let ((stcolor (visiblewave-color vw)))
    (let* ((frame (gtk-frame-new "Style"))
	   (label (gtk-label-new "Style"))
	   (hbox (gtk-hbox-new #f 5))
	   (vbox (gtk-vbox-new #f 5))
	   (wcmenu-box (build-wavecolor-menu vw 
					     (lambda (col)
					       (set! stcolor col)))))
	 (gtk-container-set-border-width frame 10)
;	 (gtk-widget-set-usize frame 200 150)
	 (gtk-widget-show frame)
	 (gtk-container-add frame hbox)
	 (gtk-box-pack-start hbox wcmenu-box #f #f 0)
	 (gtk-widget-show hbox)
	 (gtk-notebook-append-page notebook frame label)
	 )
    (lambda () 
      (set-visiblewave-color! vw stcolor)
      (dbprint "apply color " stcolor "\n"))
))

; build and attach frame for Stats page of the VisibleWave options notebook.
(define (add-vw-opts-stats-frame notebook vw)
    (let* ((frame (gtk-frame-new "Stats"))
	   (label (gtk-label-new "Stats"))
	   (vbox (gtk-vbox-new #f 5))
	   (file-label (gtk-label-new 
			(string-append "file: " 
			       (wavefile-file-name (visiblewave-file vw)))))
	   (varname-label (gtk-label-new 
			(string-append "variable: " (visiblewave-varname vw))))
	   (min-label (gtk-label-new 
			(string-append "minimum: " (number->string (wavevar-min vw)))))
	   (max-label (gtk-label-new 
			(string-append "maximum: " (number->string (wavevar-max vw))))))
      (gtk-container-set-border-width frame 10)
;      (gtk-widget-set-usize frame 200 150)
      (gtk-widget-show frame)
      (gtk-container-add frame vbox)
      (gtk-box-pack-start vbox file-label #f #f 0)
      (gtk-widget-show file-label)
      (gtk-box-pack-start vbox varname-label #f #f 0)
      (gtk-widget-show varname-label)
      (gtk-box-pack-start vbox min-label #f #f 0)
      (gtk-widget-show min-label)
      (gtk-box-pack-start vbox max-label #f #f 0)
      (gtk-widget-show max-label)
      (gtk-widget-show vbox)
      (gtk-notebook-append-page notebook frame label)
))

(define (popup-vw-options vw)
 (let* ((window (gtk-window-new 'toplevel))
       (vbox (gtk-vbox-new #f 0))
       (hbox (gtk-hbox-new #f 10))
       (vboxi (gtk-vbox-new #f 10))
       (separator (gtk-hseparator-new))
       (close (gtk-button-new-with-label "close"))
       (cancel (gtk-button-new-with-label "cancel"))
       (apply (gtk-button-new-with-label "apply"))
       (notebook (gtk-notebook-new))
       (styleproc (add-vw-opts-style-frame notebook vw)))

     (gtk-window-set-title window 
			   (string-append 
			    (wavefile-tag (visiblewave-file vw)) ":"
			    (visiblewave-varname vw) " Options"))
     (gtk-container-set-border-width window 0)
     (gtk-container-add window vbox)
     (gtk-widget-show vbox)

     (gtk-box-pack-start vbox vboxi #t #t 0)
     (gtk-container-set-border-width vboxi 10)
     (gtk-widget-show vboxi)

     (gtk-notebook-set-tab-pos notebook 'top)
     (gtk-box-pack-start vboxi notebook #t #t 0)
     (gtk-widget-show notebook)

     (add-vw-opts-stats-frame notebook vw)
 
     (gtk-box-pack-start vbox separator #f #t 0)
     (gtk-widget-show separator)
     (gtk-container-set-border-width hbox 10)
     (gtk-box-pack-start vbox hbox #f #t 0)
     (gtk-widget-show hbox)

     (gtk-signal-connect close "clicked" 
			 (lambda (x) 
			   (styleproc) 
			   (gtk-widget-destroy window)))
     (gtk-box-pack-start hbox close #t #t 0)
     (gtk-widget-show close)
     (gtk-tooltips-set-tip gwave-tooltips close 
			   "Apply changes and close options window" "")

     (gtk-box-pack-start hbox apply #t #t 0)
     (gtk-signal-connect apply "clicked" 
			 (lambda (x) (styleproc)))
     (gtk-widget-show apply)
     (gtk-tooltips-set-tip gwave-tooltips apply
			   "Apply changes to VisibleWave" "")

     (gtk-box-pack-start hbox cancel #t #t 0)
     (gtk-signal-connect cancel "clicked" 
			 (lambda (x) 
			   (gtk-widget-destroy window)))
     (gtk-widget-show cancel)
     (gtk-tooltips-set-tip gwave-tooltips cancel
			   "Close options window, discarding changes" "")
     
;     (gtk-widget-set-flags close '(can-default))
;     (gtk-widget-grab-default close)
     (gtk-widget-show window)
))

(dbprint "visiblewave-ops.scm done\n")
