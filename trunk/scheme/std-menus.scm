;
; module providing standard menus for gwave
;

(define-module (app gwave std-menus)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app gwave cmds)
  :use-module (app gwave globals)
)

(debug-enable 'debug)
(read-enable 'positions)

(dbprint "std-menus.scm running\n")

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
(define-public (add-radio-menuitem parent group label active proc)
  (let ((item (if label
		  (gtk-radio-menu-item-new-with-label group label)
		  (gtk-radio menu-item-new group))))
    (gtk-widget-show item)
    (if proc
	(gtk-signal-connect item "activate" proc))
    (if active
	(gtk-check-menu-item-set-state item active))
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
   (dbprint "in std-menus new-wavewin-hook\n")
   (let ((win (get-wavewin))
	 (mbar (get-wavewin-menubar)))
     (let ((file-menu (gtk-menu-new)))
       (gtk-widget-show file-menu)
       (gtk-menu-item-set-submenu (add-menuitem mbar "File" #f) file-menu)
       (add-menuitem file-menu "About GWave" show-about-window!)
       (add-menuitem file-menu "Read File..." 
		     (lambda () (with-selected-filename "Datafile to load"
				 (lambda (fn) (load-wavefile! fn)))))
       (add-menuitem file-menu "Export Postscript..." 
		     (lambda () (with-selected-filename 
				 "Postscript file name"
				 (lambda (fn) (export-waveimage! fn "ps"))
				 #:default "gwave_out.ps")))
       (add-menuitem file-menu "Export PNM..." 
		     (lambda () (with-selected-filename 
				 "PNM file name"
				 (lambda (fn) (export-waveimage! fn "pnm"))
				 #:default "gwave_out.pnm")))
       (add-menuitem file-menu #f #f)
       (add-menuitem file-menu "Save Configuration as Script"
		     (lambda () (with-selected-filename "Scriptfile to write"
				 (lambda (fn) (write-allrestore-script fn))
				 #:default "gwave.gw")))
       (add-menuitem file-menu "Execute Guile Script..." 
		     (lambda () (with-selected-filename 
				 "Guile script to run" execute-script)))
       (add-menuitem file-menu #f #f)
       (add-menuitem file-menu "Quit" (lambda () (gtk-main-quit)))
       )
     (let ((view-menu (gtk-menu-new)))
       (gtk-widget-show view-menu)
       (gtk-menu-item-set-submenu (add-menuitem mbar "View" #f) view-menu)
       (add-menuitem view-menu "Add Panel" 
		     (lambda () (wtable-insert-typed-panel! #f default-wavepanel-type)))
       (add-menuitem view-menu "Zoom Cursors" x-zoom-cursors!)
       (add-menuitem view-menu "Zoom X Full" x-zoom-full!)
       (add-menuitem view-menu "Zoom X..." x-zoom-area!)
       (add-menuitem view-menu "Zoom Y..." y-zoom-range!)
       (add-menuitem view-menu "Zoom XY-Area..." xy-zoom-area!)
       (set! var-list-submenu (menu-create view-menu "Variable List"))
       (add-menuitem view-menu "Redraw All" wtable-redraw!)
       )
     (let ((menu (gtk-menu-new)))
       (gtk-widget-show menu)
       (gtk-menu-item-set-submenu (add-menuitem mbar "Options" #f) menu)
;       (add-menuitem menu "foo" #f)

       (let ((ptmenu (gtk-menu-new))
	     (group #f)
	     (save-wp-type default-wavepanel-type)) ; GTK bug - first radio-menu-item gets immediate callback
	 (gtk-widget-show ptmenu)
	 (set! group (add-radio-menuitem 
		      ptmenu group (list-ref wavepanel-type-names 0)
		      (eqv? save-wp-type 0)
		      (lambda () 
			(set! default-wavepanel-type 0))))
	 (set! group (add-radio-menuitem 
		      ptmenu group (list-ref wavepanel-type-names 1)
		      (eqv? save-wp-type 1)
		      (lambda () 
			(set! default-wavepanel-type 1))))
	 (gtk-menu-item-set-submenu 
	  (add-menuitem menu "Default Panel Type" #f) ptmenu)
	 (set! default-wavepanel-type save-wp-type))

       (let ((lxmenu (gtk-menu-new))
	     (group #f))
	 (gtk-widget-show lxmenu)
	 (set! group (add-radio-menuitem 
		      lxmenu group "Linear" #t
		      (lambda () (wtable-set-xlogscale! #f))))
	 (set! group (add-radio-menuitem 
		      lxmenu group "Log" #f
		      (lambda () (wtable-set-xlogscale! #t))))
	 (gtk-menu-item-set-submenu 
	  (add-menuitem menu "X Axis Scale" #f) lxmenu))

       (let ((submenu (gtk-menu-new))
	     (group #f))
	 (gtk-widget-show submenu)
	 (set! group (add-radio-menuitem 
		      submenu group "On" (gtk-tooltips-enabled? gwave-tooltips)
		      (lambda () (gtk-tooltips-enable gwave-tooltips))))
	 (set! group (add-radio-menuitem 
		      submenu group "Off" (not (gtk-tooltips-enabled? gwave-tooltips))
		      (lambda () (gtk-tooltips-disable gwave-tooltips))))
	 (gtk-menu-item-set-submenu 
	  (add-menuitem menu "ToolTips" #f) submenu))

       (let ((submenu (gtk-menu-new)))
	 (gtk-widget-show submenu)
	 (gtk-menu-item-set-submenu 
	  (add-menuitem menu "Cursor 1 Measurement" #f) submenu)
	  (add-menuitem submenu "Cursor1 value" 
			(lambda () (set-all-measurements! 1 5)
				(set! default-measure1-function 5)))
	  (add-menuitem submenu "Cursor1 val - Cursor0 val"
			(lambda () (set-all-measurements! 1 6)
				(set! default-measure1-function 6)))
	 )
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
   (dbprint "in std-menus new-wavefile-hook " df "\n")
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
		       (rebuild-varlist-submenu!)
		       (wtable-redraw!)))
       (add-menuitem menu "Save Configuration as Script"
		     (lambda () (with-selected-filename "Scriptfile to write"
				 (lambda (fn) (write-filerestore-script df fn))
				 #:default (string-append 
					    (wavefile-file-name df) ".gw"))))
       (add-menuitem menu "Apply Script to File"
     		     (lambda () (with-selected-filename "Guile script to run"
				 (lambda (fn) (apply-script-to-file fn df)))))

       (add-menuitem menu #f #f)
       (add-menuitem menu "Close"
		 (lambda () (wavefile-remove-listwin! df)))
       )))

;
; Popup menu on button 3 in a wavepanel.  
; Note that the menu is constructed anew each time, so that it can be
; context-sensitive.  So far this hasn't produced any noticable popup delay.
;

(wavepanel-bind-mouse 3
 (lambda (wp event)
;   (display "in wavepanel menu ")(display wp) 
;   (display " type=") (display (wavepanel-type wp))   (newline)
   (let ((menu (gtk-menu-new))
	 (next-ptype (remainder (+ 1 (wavepanel-type wp)) wavepanel-num-types)))
     (gtk-widget-show menu)
     (add-menuitem menu "Zoom Cursors" x-zoom-cursors!)
     (add-menuitem menu "Zoom X..." x-zoom-area!)
     (add-menuitem menu "Zoom X Full" x-zoom-full!)
     (add-menuitem menu "Zoom Y..." y-zoom-range!)
     (add-menuitem menu "Zoom Y Full+Auto" (lambda () (y-zoom-fullauto! wp)))
     (add-menuitem menu "Zoom XY-Area..." xy-zoom-area!)
     (add-menuitem menu "Insert Panel Above" 
		   (lambda () (wtable-insert-typed-panel! wp default-wavepanel-type)))
     (add-menuitem menu "Delete this Panel"
		   (lambda () (wtable-delete-panel! wp)))
     (add-menuitem 
      menu 
      (string-append "Set type " (list-ref wavepanel-type-names next-ptype))
      (lambda () (set-wavepanel-type! wp next-ptype)))

     (if (wavepanel-ylogscale? wp)
	 (add-menuitem menu "Linear Y Scale"
		   (lambda () (set-wavepanel-ylogscale! wp #f)))
	 (add-menuitem menu "Log Y Scale"
		   (lambda () (set-wavepanel-ylogscale! wp #t))))
       
     (gtk-menu-popup menu #f #f
		     (gdk-event-button event)
		     (gdk-event-time event)))))

(dbprint "std-menus.scm done\n")
