;
; module illustrating how to add your own menus to gwave
;

(define-module (app gwave extra-menus)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app gwave cmds)
  :use-module (app gwave std-menus)
)

(debug-enable 'debug)
(read-enable 'positions)

(dbprint "extra-menus.scm running\n")

; demonstrating how we can add our own menu to the end
; of the menubar by using append-hook.
; this adds some debugging options.
(append-hook! 
 new-wavewin-hook
 (lambda ()
;   (display "in new-wavewin-hook") (newline)
   (let* ((mbar (get-wavewin-menubar))
	  (menu (menu-create mbar "Debug")))
       (add-menuitem menu "my menu" #f)
       (add-menuitem menu "garbage collect" gc)

       (add-menuitem menu "list panels" 
		     (lambda () 
		       (display "panel-list:") (newline)
		       (display (wtable-wavepanels))
		       (newline)))

       (add-menuitem menu "list visiblewaves" 
		     (lambda ()
		       (for-each (lambda (wp)
			   (display "wp:") (display wp) (newline)
			   (display (wavepanel-visiblewaves wp)) (newline))
				 (wtable-wavepanels))))

       (add-menuitem menu "list files"
		     (lambda () 
		       (display "wavefile-list:") (newline)
		       (display (wavefile-list))
		       (newline)))
)))


(append-hook!
 new-wavelist-hook
 (lambda (df)
;   (display "in extra-menus new-wavelist-hook for") (display df) (newline)
   (let* ((mbar (wavefile-listwin-menubar df))
	  (menu (menu-create mbar "Debug")))
       (add-menuitem menu "List Variables" 
		     (lambda () 
		       (display (wavefile-all-variables df))
		       (newline)
		     ))
       (add-menuitem menu #f #f)
       )))



(dbprint "extra-menus.scm done\n")
