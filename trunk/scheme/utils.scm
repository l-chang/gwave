;
; general-purpose scheme utility functions 
;

(define-module (app gwave utils)
  :use-module (ice-9 regex)
)

(debug-enable 'backtrace)
(debug-enable 'debug)

; join - a procedure like the perl function "join:"
; concatenate list of strings, putting a seperator string between each
; element of the list.
(define-public (join s l)
  (cond ((null? l)     "")
        ((= 1 (length l))     (car l))
        (else (string-append (car l) s (join s (cdr l))))))

; filter out shell metacharacters from a string
(define metachars-regexp (make-regexp "[\t <>\(\)|&;^\$]+"))
(define-public (filter-metachars s)
  (regexp-substitute/global #f metachars-regexp s 'pre 'post))


; use regular expression to find portion of string like 
; 	<dot><upper case letters><dot>
(define-public (find-dotupper s)
  (let* ((r (make-regexp "\.([A-Z][A-Z]*)\."))
	(m (regexp-exec r s)))
    (if m
	  (match:substring m 1)
	#f)))

