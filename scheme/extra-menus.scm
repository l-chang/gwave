;
; module illustrating how to add your own menus to gwave
;

(define-module (app gwave extra-menus)
  :use-module (gnome-2)
  :use-module (gnome gtk)
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
		       (for-each (lambda (wp)
				   (display wp)
				   (if (wavepanel-selected? wp)
				       (display "[selected]"))
				   (newline))
				 (wtable-wavepanels))
		       (newline)))

       (add-menuitem menu "list visiblewaves" 
		     (lambda ()
		       (for-each (lambda (wp)
			   (display "wp:") (display wp) (newline)
			   (for-each (lambda (vw)
				       (display vw)(newline))
				     (wavepanel-visiblewaves wp)))
				     (wtable-wavepanels))))

       (add-menuitem menu "list files"
		     (lambda () 
		       (format #t "wavefile-list: ~a\n" (wavefile-list))))

       (add-menuitem menu "waveform calculate test" 
		     (lambda ()
		       (display "calculate-waves stub\n")
		       (let ((wvlist (all-selected-waves)))
			 (cond 
			  ((eq? 2 (length wvlist))
			   (let* ((w1 (car wvlist))
				  (w2 (cadr wvlist))
				  (newname (format #f "~a-~a" 
						   (visiblewave-varname w1)
						   (visiblewave-varname w2))))
			     (format #t "calc ~s - ~s => ~s\n"  w1 w2 newname)
			     (new-wavevar-calc! newname - w1 w2)
			     ))

			  ((eq? 1 (length wvlist))
			   (let* ((w1 (car wvlist))
				  (newname (format #f "(-~a)" 
						   (visiblewave-varname w1))))
			     (format #t "calc - ~s => ~s\n"  w1 newname)
			     (new-wavevar-calc! newname - w1)
			     ))
			   
			  (else (display "calculate requires one or two waves.\n"))
			  ))))


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
       (add-menuitem menu "List Sweeps" 
		     (lambda () 
		       (format #t "~s sweeps:\n" (wavefile-nsweeps df))
		       (display (wavefile-sweeps df)) (newline)
		       (for-each (lambda (s)
				 (format #t " ~s = ~s\n" (car s) (cdr s)))
				 (wavefile-sweeps df))
		     ))

       (add-menuitem menu #f #f)
       )))

(dbprint "extra-menus.scm done\n")
