;
; module providing useful wrappers around common Gtk+ operations
; bundled with gwave, but does not depend on gwave.
;

(define-module (app gwave gtk-helpers)
  :use-module (gnome-2)
  :use-module (gnome gtk)
  :use-module (gnome gobject)
  :use-module (oop goops)
)

(read-set! keywords 'prefix)

; guile-gnome-platform glue
(define-public (gtk-menu? g)   		(is-a? g <gtk-menu>))
(define-public (gtk-menu-bar? g)	(is-a? g <gtk-menu-bar>))

(define-public (gtk-menu-append p i) (append p i))
(define-public (gtk-menu-bar-append p i) (append p i))

(define-public (gtk-signal-connect w sig p) 
  (gtype-instance-signal-connect w (string->symbol sig) p))

(define-public (gtk-entry-new) (make <gtk-entry>))

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
	(gtk-signal-connect item "clicked" (lambda (x) (proc))))
;			    (lambda ()
;			      (if (gtk-toggle-button-get-active item)
;				  proc))))
    (if active
	(gtk-toggle-button-set-active item #t))
    (gtk-container-add parent item) 
    (gtk-radio-button-get-group item)
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
				(lambda (x) 
				  (proc (cdr (car optlist))))))
	(gtk-widget-show item)
	(gtk-container-add container item) 
	(set! group (get-group item))
	(if (not (null? (cdr optlist)))
	    (begin
	      (add-radiobuttons-to-box container proc (cdr optlist))))))

    (add-radiobuttons-to-box container proc optlist)
    (gtk-widget-show container)
    container))

; build and return an option-menu button
; this is a combobox, that drops down a menu when a button is pressed;
; we return the top-level widget.
; args: 
; 	proc - called with new value on each change selection.
;	optlist - list of pairs    ( (label1 . value1) (label2 . value2) ... )
(define-public (build-option-menu proc optlist)
  (let ((combobox (gtk-combo-box-new-text)))
    (for-each (lambda (opt)
		(append-text combobox (car opt)))
	      optlist)
    (if (procedure? proc)
	(connect combobox 'changed 
		 (lambda (x)
		   (proc (cdr (list-ref optlist (get-active combobox) ))))))
    (set-active combobox 0)
    (show combobox)
    combobox))
