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

