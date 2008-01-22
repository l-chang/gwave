;
; module providing standard "toolbar" buttons for gwave
;

(define-module (app gwave std-toolbar)
  :use-module (gnome-0)
  :use-module (gnome gtk)
  :use-module (app gwave cmds)
  :use-module (app gwave globals)
  :use-module (app gwave gtk-helpers)
)

(dbprint "std-toolbar.scm running\n")
(set! gwave-std-toolbar-loaded #t)

(define (button label tip proc)
  (let ((item (gtk-button-new-with-label label)))
    (gtk-widget-show item)
    (if proc
	(gtk-signal-connect item "clicked" 
			    (lambda (b) (proc))))
    (if tip
	(gtk-tooltips-set-tip gwave-tooltips item tip ""))
    item))

(add-hook! 
 new-wavewin-hook
 (lambda ()
;   (display "in std-toolbar new-wavewin-hook") (newline)
   (let ((tbar (get-wavewin-toolbar)))
     (gtk-container-add tbar (button "Zoom In" #f
				     (lambda () (x-zoom-relative! 2))))
     (gtk-container-add tbar (button "Zoom Out"  #f
				     (lambda () (x-zoom-relative! 0.5))))
     (gtk-container-add tbar (button "Delete" "Delete selected waves"
				     (lambda () (delete-selected-waves!))))
     (gtk-container-add tbar (button "Reload All" 
				     "Reread all waveform data files"
				     reload-all-files!))
)))

(dbprint "std-toolbar.scm done\n")
