;
; globals for the scheme-based part of the UI.
; seperated out 
;

(define-module (app gwave globals)
  :use-module (gtk gtk)
)

(define-public gwave-tooltips #f)
(if (not gwave-tooltips)
      (set! gwave-tooltips (gtk-tooltips-new)))

