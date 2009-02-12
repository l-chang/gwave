
(dbprint "gwave-startup.scm running\n")
; gwave-startup.scm - this is called with (load ...) from minimal.scm
; 
; Outline:
; 1. whatever setup is required before we can load a .gwaverc
; 2. find and load a .gwaverc
; 3. load fallback stuff if the user's .gwaverc omitted important things.
;
; The point of #3 is to allow both "simple" user .gwaverc's
; that only set some configuration variables, and also "full-blown" ones
; where we assume the user really knows what they're doing and will
; take care of all gwave-specific initializtion that they want, including
; loading of specific gwave core modules.
;

(use-modules 
 (gnome-2)
 (gnome gtk)
 (app gwave cmds)
)
(define gwave-std-toolbar-loaded #f)
(define gwave-std-args-loaded #f)
(define gwave-std-menus-loaded #f)


; Variables that can get set or altered in .gwaverc
(define-public gwave-tooltips (gtk-tooltips-new))
(set-wtable-tooltips! gwave-tooltips)
(define default-wavepanel-type 0)
(define gwave-no-std-toolbar #f)
(define gwave-no-std-menus #f)
(define gwave-no-std-args #f)
(define initial-panels 2)
(define default-measure1-function 5)

;
; Find a .gwaverc file to load, loading only the first one found.
; I'm not sure this is quite the model I want:
; since the program is pretty useless without getting a bunch of things loaded,
; perhaps they all should be loaded, allowing things to append and override.
; That requires figuring out how to make the stuff in system.gwaverc more
; flexibile though.
;
(let ((home-gwaverc (string-append (getenv "HOME") "/.gwaverc"))
      (system-gwaverc (string-append gwave-datadir
				     "/guile/app/gwave/system.gwaverc")))
  (if (access? "./.gwaverc" R_OK)
      (safe-load "./.gwaverc")
      (if (access? home-gwaverc R_OK)
	  (safe-load home-gwaverc)
	  (if (access? system-gwaverc R_OK)
	      (safe-load system-gwaverc)))))

;
; Fallbacks if .gwaverc didn't do much - this usually means user had one,
; but it didn't load any modules.
;

(if (and (not gwave-std-args-loaded)
	 (not gwave-no-std-args))
    (use-modules (app gwave std-args)))
		 
(if (and (not gwave-std-menus-loaded)
	 (not gwave-no-std-menus))
    (use-modules (app gwave std-menus)
		 (app gwave visiblewave-ops) ))

(if (and (not gwave-std-toolbar-loaded)
	 (not gwave-no-std-toolbar))
    (use-modules (app gwave std-toolbar)))

(use-modules (app gwave export-gnuplot))
(use-modules (app gwave export-gnugraph))

(dbprint "gwave-startup.scm done\n")

