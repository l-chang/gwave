;
; module providing some simple gwave commands
;

(define-module (app gwave cmds)
  :use-module (gtk gtk)
)
(read-set! keywords 'prefix)

(define-public (append-hook! hook proc)
  "Add PROC to HOOK at the end of the list."
  (add-hook! hook proc #t))

; x-axis zoom in/zoom out zoom by this ammount
(define x-zoom-fraction 2)

(define-public (x-zoom-full!)
  (x-zoom! (wtable-min-xval) (wtable-max-xval)))

(define-public (x-zoom-area!)
  (select-range-x 
   (lambda (wp x1 x2)
;     (display "zoom-area-callback ")
;     (display wp) (display " ")
;     (display x1) (display " ")
;     (display x2) (newline)
     (x-zoom! (wavepanel-x2val wp x1) (wavepanel-x2val wp x2)))))

; zoom relative to current position.
; zf>1 zooms in
; zf<1 zooms out
(define-public (x-zoom-relative! zf)
  (let* ((sx (wtable-start-xval))
	 (ex   (wtable-end-xval))
	 (center ( / (+ sx ex) 2))
	 (width (- ex sx)))
    (x-zoom! ( - center (/ width (* zf 2)))
	   ( + center (/ width (* zf 2))))))

; zoom X so that edges of displayed are where the vertical cursors are now.
; If both vertical bar cursors aren't displayed, do nothing.
;
; FIXME:tell:
;  pop message somewhere if both cursors not displayed
;  zoom so that cursors are just visible at edges, say 5% in from edge.
(define-public (x-zoom-cursors!)
  (let ((c0 (wtable-vcursor 0))
	(c1 (wtable-vcursor 1)))
    (if (and c0 c1)
	(x-zoom! c0 c1))))


; Make a simple button with a textual label
(define (make-button parent txt func) 
  (let* ((btn (gtk-button-new-with-label txt)))
    (gtk-container-add parent btn)
    (gtk-widget-show btn)
    (if func (gtk-signal-connect btn "clicked" func))
    btn))

;
; create and show a top-level window 
; with the "about" information
;
(define-public (show-about-window!)
  (let* ((window (gtk-widget-new 'GtkWindow
				 :type         'toplevel
				 :title        "About Gwave"
				 :GtkContainer::border_width 10))
	 (vbox (gtk-vbox-new #f 10)))
    (gtk-widget-show vbox)
    (gtk-container-add window vbox)
    (let ((llab (gtk-label-new 
		 (string-append "gwave version " gwave-version-string))))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))
    (let ((llab (gtk-label-new "by Steve Tell")))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))
    (let ((llab (gtk-label-new "tell@cs.unc.edu")))
      (gtk-widget-show llab)
      (gtk-container-add vbox llab))
    (make-button vbox "Close" (lambda () (gtk-widget-destroy window)))
    (gtk-widget-show window)))

(define-public (prompt-name-load-file)
  (let* ((window (gtk-file-selection-new "file selection"))
         (button #f))
    (gtk-signal-connect
     (gtk-file-selection-ok-button window)
     "clicked" (lambda () 
;		 (display (gtk-file-selection-get-filename window)) (newline)
		 (load-wavefile! (gtk-file-selection-get-filename window))
		 (gtk-widget-destroy window)
		 ))
			  
    (gtk-signal-connect 
     (gtk-file-selection-cancel-button window)
     "clicked" (lambda () (gtk-widget-destroy window)))

    (gtk-file-selection-hide-fileop-buttons window)

; TODO: put a selector for file type in the action-area
;    (set! button (gtk-button-new-with-label "Show Fileops"))
;    (gtk-signal-connect 
;     button "clicked"
;     (lambda () (gtk-file-selection-show-fileop-buttons window)))
;    (gtk-box-pack-start (gtk-file-selection-action-area window)
;                        button #f #f 0)
;    (gtk-widget-show button)

    (gtk-widget-show window)
))
