;
; module providing GUI operations on VisibleWave objects
;   popup menu on right button in VisibleWave-button
;   multi-paneled dialog box accessible from the menu
;

(dbprint "visiblewave-ops.scm running\n")

(define-module (app gwave visiblewave-ops)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app gwave std-menus)
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

   (gtk-signal-connect (visiblewave-button vw) "button-press-event" 
			(lambda (event) 
;			  (display "press-signal") 
;			  (display event)
;			  (display (gdk-event-type event))
;			  (display " ")
;			  (display (gdk-event-button event))
;			  (newline)
			  (if (= (gdk-event-button event) 3)
			      (gtk-menu-popup (make-vwb3-menu vw) #f #f
					      (gdk-event-button event)
					      (gdk-event-time event)))))
   (gtk-tooltips-set-tip gwave-tooltips (visiblewave-button vw) 
			 "VisibleWave Button:\nClick button 1 to select wave.\nPress button 3 for options menu." "")

))

; create dynamic menu to be popped up with mousebutton 3 on the 
; visible-wave button.  Returns the menu.
(define (make-vwb3-menu vw)
  (let ((menu (gtk-menu-new)))
    (gtk-widget-show menu)
    (add-menuitem menu "Wave Options"
		  (lambda () (popup-vw-options vw)))
    (add-menuitem menu "Delete" 
		  (lambda () (visiblewave-delete! vw)))
    menu))

; build and return wave-color color menu.
; this is an optionmenu on a button; we return the button.
; call proc with new value on menu selection.
(define (build-wavecolor-menu vw proc)
  (let ((menu (gtk-menu-new))
	(group #f)
	(vbox (gtk-vbox-new #f 0))
	(optionmenu (gtk-option-menu-new))
	(fixed (gtk-fixed-new)))

    (do ((i 0 (1+ i))
	 (j 0 (1+ j)))
	((= i 6))
      (let* ((label (gtk-label-new 
		     (string-append "color " (number->string j))))
	     (menuitem (gtk-radio-menu-item-new group))
	     (eventbox (gtk-event-box-new)))
	(gtk-widget-set-name label 
			     (string-append "wavecolor" (number->string j)))
	(gtk-container-add menuitem eventbox)
	(gtk-container-add eventbox label)
	(gtk-widget-show label)
	(gtk-widget-set-name eventbox "wavebutton")
	(gtk-widget-show eventbox)
	(set! group menuitem)
	(gtk-menu-append menu menuitem)
	(gtk-widget-show menuitem)
	(gtk-signal-connect menuitem "toggled"
			    (lambda () 
			      ; (if (menuitem is active) ;how to find this?
				  (begin
				    (proc j))))
	))
	
    (gtk-widget-show menu)

    (gtk-option-menu-set-menu optionmenu menu)
    (gtk-option-menu-set-history optionmenu (visiblewave-color vw))
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
	 (gtk-container-border-width frame 10)
	 (gtk-widget-set-usize frame 200 150)
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
	   (min-label (gtk-label-new 
			(string-append "minimum: " (number->string (wavevar-min vw)))))
	   (max-label (gtk-label-new 
			(string-append "maximum: " (number->string (wavevar-max vw))))))
      (gtk-container-border-width frame 10)
      (gtk-widget-set-usize frame 200 150)
      (gtk-widget-show frame)
      (gtk-container-add frame vbox)
      (gtk-box-pack-start vbox file-label #f #f 0)
      (gtk-widget-show file-label)
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
     (gtk-container-border-width window 0)
     (gtk-container-add window vbox)
     (gtk-widget-show vbox)

     (gtk-box-pack-start vbox vboxi #t #t 0)
     (gtk-container-border-width vboxi 10)
     (gtk-widget-show vboxi)

     (gtk-notebook-set-tab-pos notebook 'top)
     (gtk-box-pack-start vboxi notebook #t #t 0)
     (gtk-widget-show notebook)

     (add-vw-opts-stats-frame notebook vw)
 
     (gtk-box-pack-start vbox separator #f #t 0)
     (gtk-widget-show separator)
     (gtk-container-border-width hbox 10)
     (gtk-box-pack-start vbox hbox #f #t 0)
     (gtk-widget-show hbox)

     (gtk-signal-connect close "clicked" 
			 (lambda () 
			   (styleproc) 
			   (gtk-widget-destroy window)))
     (gtk-box-pack-start hbox close #t #t 0)
     (gtk-widget-show close)
     (gtk-tooltips-set-tip gwave-tooltips close 
			   "Apply changes and close options window" "")

     (gtk-box-pack-start hbox apply #t #t 0)
     (gtk-signal-connect apply "clicked" 
			 (lambda () (styleproc)))
     (gtk-widget-show apply)
     (gtk-tooltips-set-tip gwave-tooltips apply
			   "Apply changes to VisibleWave" "")

     (gtk-box-pack-start hbox cancel #t #t 0)
     (gtk-signal-connect cancel "clicked" 
			 (lambda () 
			   (gtk-widget-destroy window)))
     (gtk-widget-show cancel)
     (gtk-tooltips-set-tip gwave-tooltips cancel
			   "Close options window, discarding changes" "")
     
     (gtk-widget-set-flags close '(can-default))
     (gtk-widget-grab-default close)
     (gtk-widget-show window)
))

(dbprint "visiblewave-ops.scm done\n")
