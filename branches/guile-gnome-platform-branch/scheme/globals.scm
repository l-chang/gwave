;
; globals for the scheme-based part of the UI.
; seperated out 
;

(dbprint "globals.scm running\n")

(define-module (app gwave globals)
  :use-module (gnome-0)
  :use-module (gnome gtk)
)

;(define-public gwave-tooltips #f)
;(if (not gwave-tooltips)
;      (set! gwave-tooltips (gtk-tooltips-new)))

(dbprint "globals.scm done\n")
