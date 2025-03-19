(define-library
    (bc)
  (import (scheme base) (scheme write) (scheme file) (expand) (library-manager)
	  (scheme process-context)
	  (util) (match)
	  (passes)
	  (srfi 1)
	  (srfi 69)
	  (srfi 151)
	  (format))
  (export compile link)
  (cond-expand (gauche
		(import (only (binary io) write-f64 read-u64))
		(begin
		  (define (get-double-as-u64 d)
		    (let ((p (open-output-string)))
		      (write-f64 d p)
		      (let ((p (open-input-string (get-output-string p))))
			(read-u64 p))))
		  (define (link output output-file opts)
		    ;; Bootstrap does not link, must run the command yourself.
		    #f)))
	       (callcc
		(import (prefix (flow sys) sys:)
			(only (flow all) system))
		(begin
		  (define (get-double-as-u64 x) (sys:FOREIGN_CALL "SCM_DOUBLE_AS_U64" x))
		  (define (link output output-file opts)
		    (system (get-link-command output output-file opts)))))
	       (else ))
  (include "qq.scm")
  (include "memory_layout.scm")
  (include "bc.scm"))
