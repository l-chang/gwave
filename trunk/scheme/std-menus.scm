;
; module providing standard menus for gwave
;

(define-module (app gwave std-menus)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app gwave cmds)
  :use-module (app gwave options)
)

(debug-enable 'debug)
(read-enable 'positions)

(display "std-menus.scm running\n")

;(set! default-wavepanel-type 0)
(define-public default-wavepanel-type 0)


;*****************************************************************************
;
; create a menuitem that calls proc when selected,
; add it to the parent widget, and return it just in case the caller needs
; to do somthing else with it too.
;
(define-public (add-menuitem parent label proc)
  (let ((item (if label
		  (gtk-menu-item-new-with-label label)
		  (gtk-menu-item-new))))
    (gtk-widget-show item)
    (if proc
	(gtk-signal-connect item "activate" proc))
    (cond ((gtk-menu? parent) (gtk-menu-append parent item))
	  ((gtk-menu-bar? parent) (gtk-menu-bar-append parent item)))
    item))

;
; create an empty menu, along with the menuitem that acts as its title
; and optionaly attach the title to a parent object:
;   If the parent is a menu, calls gtk-menu-append
;   If the parent is a menubar, calls gtk-menu-bar-append
; returns the menu.
;
(define-public (menu-create parent label)
  (let ((menu (gtk-menu-new)))
    (gtk-widget-show menu)
    (gtk-menu-item-set-submenu (add-menuitem parent label #f) menu)
    menu))

;
; create a gtk-radio-menu-item that calls proc when selected,
; in a fashion very much like add-menuitem. 
; add it to the parent widget, and return it.
; Caller must still handle threading up the groups.
; TODO: write a function that takes a list of lists of labels & procs,
; and creates the whole set of radio-menu-items.
;
(define-public (add-radio-menuitem parent group label proc)
  (let ((item (if label
		  (gtk-radio-menu-item-new-with-label group label)
		  (gtk-radio menu-item-new group))))
    (gtk-widget-show item)
    (if proc
	(gtk-signal-connect item "activate" proc))
    (cond ((gtk-menu? parent) (gtk-menu-append parent item))
	  ((gtk-menu-bar? parent) (gtk-menu-bar-append parent item)))
    item))


;*****************************************************************************
; globals and ancilary procedures related to the menus

(define var-list-submenu #f)

(define (rebuild-varlist-submenu!)
  (for-each (lambda (mitem)
	      (gtk-container-remove var-list-submenu mitem))
	    (gtk-container-children var-list-submenu))
  (for-each (lambda (df)
   (add-menuitem var-list-submenu 
		 (string-append (wavefile-tag df)
				": "
				(wavefile-file-name df))
		 (lambda () (wavefile-show-listwin! df))))
	    (wavefile-list)))

;*****************************************************************************
; construct the actual menus for gwave.
;

; hook called when main window opened.  Main purpose is creating the menus.
(add-hook! 
 new-wavewin-hook
 (lambda ()
   (display "in std-menus new-wavewin-hook") (newline)
   (let ((win (get-wavewin))
	 (mbar (get-wavewin-menubar)))
     (let ((file-menu (gtk-menu-new)))
       (gtk-widget-show file-menu)
       (gtk-menu-item-set-submenu (add-menuitem mbar "File" #f) file-menu)
       (add-menuitem file-menu "About GWave" show-about-window!)
       (add-menuitem file-menu "Read File..." prompt-name-load-file)
       (add-menuitem file-menu "Export Postscript" 
		     (lambda () (export-waveimage! "gwave_out.ps" "ps")))
       (add-menuitem file-menu "Export PNM" 
		     (lambda () (export-waveimage! "gwave_out.pnm" "pnm")))
       (add-menuitem file-menu "Read File..." prompt-name-load-file)
       (add-menuitem file-menu #f #f)
       (add-menuitem file-menu "Quit" (lambda () (gtk-main-quit)))
       )
     (let ((view-menu (gtk-menu-new)))
       (gtk-widget-show view-menu)
       (gtk-menu-item-set-submenu (add-menuitem mbar "View" #f) view-menu)
       (add-menuitem view-menu "Add Panel" 
		     (lambda () (wtable-insert-panel! #f default-wavepanel-type)))
       (add-menuitem view-menu "Zoom Full" x-zoom-full!)
       (add-menuitem view-menu "Zoom Cursors" x-zoom-cursors!)
       (add-menuitem view-menu "Zoom Area..." x-zoom-area!)
       (set! var-list-submenu (menu-create view-menu "Variable List"))
       (add-menuitem view-menu "Redraw All" wtable-redraw!)
       )
     (let ((menu (gtk-menu-new)))
       (gtk-widget-show menu)
       (gtk-menu-item-set-submenu (add-menuitem mbar "Options" #f) menu)
;       (add-menuitem menu "foo" #f)
       (let ((ptmenu (gtk-menu-new))
	     (group #f))
	 (gtk-widget-show ptmenu)
	 (set! group (add-radio-menuitem 
		      ptmenu group "Std" 
		      (lambda () (set! default-wavepanel-type 0))))
	 (set! group (add-radio-menuitem 
		      ptmenu group "Jge"
		      (lambda () (set! default-wavepanel-type 1))))
	 (gtk-menu-item-set-submenu 
	  (add-menuitem menu "Default Panel Type" #f) ptmenu))
       )
)))

;
; new-wavefile hook: add item to the variable-list menu for the file.
; TODO: work out a way to remove the item when the file goes away.
; possible implmentations:
;   a. add a deleting-wavefile hook
;   b. move the wavelist window (or at least its menu) into scheme,
;     so we can do the delete from its menu
;   c. make this menu dynamic (can gtk+ do that?) so it gets created
;     correctly each time.
;   d. design a nicer GUI for selecting wave files and variables for display;
;     something that takes into account multiple sweeps would be nice.
;     One of the gtk tree lists might be nice. 
;     But guile-gtk can't do them yet.
;
; B has been implemented.
;
(add-hook! 
 new-wavefile-hook
 (lambda (df)
   (display "in std-menus new-wavefile-hook for ") (display df) (newline)
   (add-menuitem var-list-submenu 
		 (string-append (wavefile-tag df)
				": "
				(wavefile-file-name df))
		 (lambda () (wavefile-show-listwin! df)))
))

(add-hook!
 new-wavelist-hook
 (lambda (df)
;   (display "in std-menus new-wavelist-hook for") (display df) (newline)
   (let* ((mbar (wavefile-listwin-menubar df))
	  (menu (menu-create mbar "File")))
       (add-menuitem menu "Reload this File" 
		     (lambda () (wavefile-reload! df)))
       (add-menuitem menu "Delete this File"
		     (lambda () 
		       (wavefile-delete! df)
		       (rebuild-varlist-submenu!)))
       (add-menuitem menu #f #f)
       (add-menuitem menu "Close"
		 (lambda () (wavefile-remove-listwin! df)))
;       (add-menuitem menu "foo" #f)
       )))

;
; Popup menu on button 3 in a wavepanel.
;

(wavepanel-bind-mouse 3
 (lambda (wp event)
;   (display "in wavepanel-mouse ")(display wp)(display event) (newline)
   (let ((menu (gtk-menu-new)))
     (gtk-widget-show menu)
     (add-menuitem menu "Zoom Cursors" x-zoom-cursors!)
     (add-menuitem menu "Zoom Area..." x-zoom-area!)
     (add-menuitem menu "Zoom Full" x-zoom-full!)
     (add-menuitem menu "Insert Panel Above" 
		   (lambda () (wtable-insert-panel! wp default-wavepanel-type)))
     (add-menuitem menu "Delete this Panel"
		   (lambda () (wtable-delete-panel! wp)))

     (case (wavepanel-type wp)
       ((0) (add-menuitem menu "Set type jge"
			  (lambda () (set-wavepanel-type! wp 1))))
       ((1) (add-menuitem menu "Set type std"
			  (lambda () (set-wavepanel-type! wp 0)))))
	       
     (gtk-menu-popup menu #f #f
		     (gdk-event-button event)
		     (gdk-event-time event)))))

(display "std-menus.scm done\n")
