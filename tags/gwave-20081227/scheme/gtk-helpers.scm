;
; module providing useful wrappers around common Gtk+ operations
; bundled with gwave, but does not depend on gwave.
;

(define-module (app gwave gtk-helpers)
  :use-module (app gwave std-menus)
  :use-module (gtk gtk)
)

(read-set! keywords 'prefix)

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


; build and return a box containing a set of radio buttons.
; The box is returned.
; args: 
;	container - vbox or hbox to add the buttons to
; 	proc - called with new value on radiobutton selection.
;	optlist - list of pairs    ( (label1 . value1) (label2 . value2) ... )
(define-public (build-radiobutton-box container proc optlist)
  (let ((group #f))
;    (dbprint "build-radiobutton-box optlist=" optlist "\n")
    (define (add-radiobuttons-to-box cont proc optlist)
;      (format #t "add-radiobutton-to-box ~s ~s\n" 
;	      (car (car optlist)) (cdr (car optlist)))
      (let ((item (gtk-radio-button-new-with-label group (car (car optlist)))))
	(if proc
	    (gtk-signal-connect item "clicked"
				(lambda () 
				  (proc (cdr (car optlist))))))
;	(if active
;	    (gtk-toggle-button-set-state item active))
	(gtk-widget-show item)
	(gtk-container-add container item) 
	(set! group item)
	(if (not (null? (cdr optlist)))
	    (begin
	      (add-radiobuttons-to-box container proc (cdr optlist))))))

    (add-radiobuttons-to-box container proc optlist)
    (gtk-widget-show container)
    container))

; build and return an option-menu button
; this is an optionmenu that drops down when a button is pressed;
; we return the button.
; args: 
; 	proc - called with new value on menu selection.
;	optlist - list of pairs    ( (label1 . value1) (label2 . value2) ... )
(define-public (build-option-menu proc optlist)
  (let ((menu (gtk-menu-new))
	(group #f)
	(optionmenu (gtk-option-menu-new))
	(vbox (gtk-vbox-new #f 0))
	(fixed (gtk-fixed-new)))

    (define (add-optlist-to-menu menu proc optlist)
      (let* ((label (gtk-label-new ""))
	     (menuitem (hack-gtk-radio-menu-item-new group))
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

