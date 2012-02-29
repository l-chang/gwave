;
; waveform-list window using list-store
;

(define-module (app gwave wavelist-ls)
  :use-module (gnome-2)
  :use-module (oop goops)
  :use-module (gnome-2)
  :use-module (gnome gobject)
  :use-module (gnome glib)
  :use-module (gnome gtk)
  :use-module (srfi srfi-1)
)

(debug-enable 'debug)
(read-enable 'positions)
(read-set! keywords 'prefix)

(dbprint "wavelist-ls.scm running\n")


;; add data to the list store
(define (populate-model store df)
  (let ((idx 0))
	 (for-each 
	  (lambda (wv)
	    (let ((iter (gtk-list-store-append store))
		 (sweepname (variable-sweepname wv))
		 (sweepval 0)
		 (sweepno  (variable-sweepindex wv)))
	      (set-value store iter 0 idx)
	      (set-value store iter 1 (variable-signame wv))
	      (set-value store iter 2 sweepname)
	      (set-value store iter 3 sweepno)
	      (set! idx (+ 1 idx))))
	  (wavefile-all-variables df))


))

(define (send-selected-to-wavewin treeview df)
  (let* ((ts (get-selection treeview))
	 (n (gtk-tree-selection-count-selected-rows ts))
	 (model (gtk-tree-view-get-model treeview))
	 (iter (gtk-tree-model-get-iter-first model))
	 )
    (format #t "nselected=~s\n" n)

    (while iter
	   (let ((nxt (gtk-tree-model-iter-next model iter))
		 (index (get-value model iter 0))
		 (varname (get-value model iter 1))
		 (sweepno (get-value model iter 3))
		 )
	     (if (gtk-tree-selection-iter-is-selected ts iter)
		 (begin
		   (format #t "add ~s(~s)\n" varname index)
		   (wavepanel-add-variable! #f (wavefile-variable df varname sweepno))))
	     (set! iter nxt)
	     ))
      (unselect-all ts)
))

(define (add-columns treeview)
  (let* ((model    (get-model treeview))

	 ;; column for original datafile column-index
	 (renderer1 (make <gtk-cell-renderer-text>))
	 (column1   (make <gtk-tree-view-column>
		      :title "Index"))
	 ;; column for variable-name
	 (renderer2 (make <gtk-cell-renderer-text>))
	 (column2   (make <gtk-tree-view-column>
		      :title "Name"))

	 ;; column for sweep condition
	 (renderer3 (make <gtk-cell-renderer-text>))
	 (column3   (make <gtk-tree-view-column>
		      :title "Sweep"))
	 )

    (pack-start column1 renderer1 #f)
    (add-attribute column1 renderer1 "text" 0)
    (set-sort-column-id column1 0)
    (append-column treeview column1)

    (pack-start column2 renderer2 #f)
    (add-attribute column2 renderer2 "text" 1)
    (set-sort-column-id column2 1)
    (append-column treeview column2)

    (pack-start column3 renderer3 #f)
    (add-attribute column3 renderer3 "text" 2)
    (set-sort-column-id column3 2)
    (append-column treeview column3)
))

(define-public (show-wavelist-ls df)
  (let* (
	 ;; create window, etc
	 (window   (make <gtk-window> 
		     :type 'toplevel 
		     :title (format #f "gwave: ~s" (wavefile-file-name df))
		     :default-width 200 :default-height 350 :border-width 8))
	 (vbox     (make <gtk-vbox> :homogeneous #f :spacing 8))

	 (button (make <gtk-button> #:label "Add to Current Panel"))

	 (sw       (make <gtk-scrolled-window>
		     :hscrollbar-policy 'never :vscrollbar-policy 'automatic
		     :shadow-type 'etched-in))
	 ;; create list store
	 (model    (gtk-list-store-new (list <guint>
					     <gchararray>
					     <gchararray>
					     <guint> )))
	 ;; create tree view
	 (treeview (make <gtk-tree-view> 
		     :model model :rules-hint #t :search-column 3)))
    (populate-model model df)
    (set-mode (get-selection treeview) 'multiple)

    (add window vbox)

    (pack-start vbox button  #f #f 0)
    (connect button 'clicked (lambda (w) 
			       (display "doing it\n") 
			       (send-selected-to-wavewin treeview df) ))

    (pack-start vbox sw #t #t 0)
    
    (add sw treeview)

    ;; add columns to the tree view
    (add-columns treeview)

    (show-all window)

    (if (> 0 (wavefile-nsweeps df)) ; hide sweep column if only one
	(let ((c2 (gtk-tree-view-get-column treeview 2)))
	  (display "toggle severity vis\n") 
	  (gtk-tree-view-column-set-visible c2 #f)))

))

(dbprint "wavelist-ls.scm done\n")
