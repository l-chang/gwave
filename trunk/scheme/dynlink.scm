;; -*- scheme -*-

(display "in sgt's hacked dynlink.scm\n")

(debug-enable 'debug)
(debug-enable 'backtrace)
(read-enable 'positions)

(define-module (gtk dynlink)
  :use-module (gtk config)
  :use-module (ice-9 regex)
  :use-module (ice-9 debug)
)

(define (update-registered-modules)
  (set! registered-modules 
	(append! (convert-c-registered-modules #f)
		 registered-modules)))
;
; my attempt at using guile's own dynamic-libary stuff from boot-9.
;
(define-public (merge-compiled-code init-func libname)
  (let* ((module (current-module))
	 (interface (module-public-interface module))
	 (libnamenolib (make-shared-substring libname 3)))
    ;; make the new primitives visible from within the current module.
    (module-use! module interface) ; XXX - is this safe?
    (save-module-excursion
     (lambda ()
       (update-registered-modules)
       (set-current-module interface)

       (display "new merge-compiled-code ")
       (display libnamenolib)(display " ")(display init-func)(newline)

       (let* ((modname (list 'gtk '%static-initfuncs%
			     (string->symbol init-func)))
	      (modinfo (or-map (lambda (modinfo)
				 (if (equal? (car modinfo) modname)
				     modinfo
				     #f))
			       registered-modules))
	      (init-func (if modinfo (cadr modinfo) init-func))

	      (sharlib-full (try-using-libtool-name 
			     "/usr/local/contrib/moderated/lib" libname))

;	      (lib       (if modinfo (caddr modinfo)
;			     (or (link-dynamic-module sharlib-full init-func)
;				 (error "can't open library" libname)))))

; link-dynamic-module never returns anything.
	      )
	 (display "sharlibfull is ") (display sharlib-full)(newline)
	 (link-dynamic-module sharlib-full init-func)

;	 (display "lib is ") (display lib)(newline)
	 (display "modinfo is ") (display modinfo)(newline)

)))))

(define default-module-prefix 
  (string->symbol (string-append "gtk-" gtkconf-version)))
(define module-prefix #f)

(define-public (gtk-version-set prefix)
  (if (and module-prefix (not (eq? prefix module-prefix)))
      (error "Can't mix" module-prefix 'and prefix)
      (set! module-prefix prefix)))

(define-public (gtk-version-alias suffix)
  (if (not module-prefix)
      (set! module-prefix default-module-prefix))
;  (display "module-prefix is ")(display module-prefix)(newline)
  (let* ((mod-name (list module-prefix suffix))
	 (mod-iface (resolve-interface mod-name)))
    (or mod-iface
	(error "no such module" mod-name))
    (set-module-public-interface! (current-module) mod-iface)))
