;
; module providing standard "toolbar" buttons for gwave
;

(define-module (app gwave std-toolbar)
  :use-module (gtk gtk)
  :use-module (app gwave cmds))

(dbprint "std-toolbar.scm running\n")

(define (button label proc)
  (let ((item (gtk-button-new-with-label label)))
    (gtk-widget-show item)
    (if proc
	(gtk-signal-connect item "clicked" proc))
    item))

(add-hook! 
 new-wavewin-hook
 (lambda ()
;   (display "in std-toolbar new-wavewin-hook") (newline)
   (let ((tbar (get-wavewin-toolbar)))
     (gtk-container-add tbar (button "Zoom In" 
				     (lambda () (x-zoom-relative! 2))))
     (gtk-container-add tbar (button "Zoom Out" 
				     (lambda () (x-zoom-relative! 0.5))))
     (gtk-container-add tbar (button "Delete" 
				     (lambda () (delete-selected-waves!))))
     (gtk-container-add tbar (button "Reload All" reload-all-files!))
)))

(dbprint "std-toolbar.scm done\n")
