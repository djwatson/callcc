(import (scheme base)
	(scheme write) 
	(util)
	(scheme file)
	(srfi 1)
	(match)
	(srfi 37)
	(scheme process-context)
	(scheme time)
	(format)
	(library-manager)
	(scheme eval)
	(scheme repl)
	(scheme read)
	(scheme case-lambda)
	(scheme load)
	(bc))

;; Main compiler driver.  Argument parsing, repl + static compiler setup.

(define exe-file #f)
(define script-file #f)
(define output-file #f)
(define cc-opts "")
(define include-eval #t)

(define (print-version)
  (display "callcc v0.1 (c) 2025 Dave Watson ")
  (newline))

(define (print-version-and-exit . unused)
  (print-version)
  (exit))

(define (print-usage-and-exit . unused)
  (display "Usage: callcc [options]

Options:
  -v, --version              print the version and exit
  -I <directory>             prepend directory to list of library paths to search
  -A <directory>             append directory to list of library paths to search
  -D <identifier>            declare identifier as supported feature in (features) and cond-expand
  --cc <opts>                CC optimizations (consider -O3 -flto, or -g)
  -h, --help                 print this help and exit
  --exe <filename>           compile file to an executable
  -s, --script <filename>    run the script as if by (begin (load \"filename\")(exit)) 
  -fno-eval                  don't include eval support in generated executable
  -o, <filename>             in conjunction with --exe, set the output filename
  --                         pass through remaining args

If neither --exe nor -s is specified, enters a read/eval/print loop (REPL)
")
  (exit))

(define (prepend-directory o n a s)
  (set! library-search-paths (cons a library-search-paths)))
(define (append-directory o n a s)
  (set! library-search-paths (append library-search-paths (list a))))
(define (add-feature-flag o n a s)
  (add-feature (string->symbol a)))
(define (set-exe-file o n a s)
  (when exe-file (error "Multiple exe files found"))
  (set! exe-file a))
(define (set-script-file o n a s)
  (when script-file (error "Multiple scripts found"))
  (set! script-file a))
(define (set-output-file o n a s)
  (set! output-file a))
(define (set-cc-opts o n a s)
  (set! cc-opts a))
(define (compile-option o n a s)
  (let ((val (string->symbol a)))
    (case val
      ((no-eval) (set! include-eval #f))
      (else (error "Unknown compiler option: -f" val)))))

(args-fold (cdr (command-line))
	   (list
	    (option '(#\v "version") #f #f print-version-and-exit)
	    (option '(#\h "help") #f #f print-usage-and-exit)
	    (option '(#\I) #t #f prepend-directory)
	    (option '(#\A) #t #f append-directory)
	    (option '(#\D) #t #f add-feature-flag)
	    (option '("exe") #t #f set-exe-file)
	    (option '("cc") #t #f set-cc-opts)
	    (option '(#\f) #t #f  compile-option)
	    (option '(#\s "script") #t #f set-script-file)
	    (option '(#\o) #t #f set-output-file))
	   (lambda (o n a s) (error "Unrecognized option:" n))
	   (lambda (o s) (error "Unknown field: " o))
	   '())

(define (repl)
  (display "> ") (flush-output-port)
  (let* ((sexp (guard (obj (else (display "Extra list terminator found") #f)) (read)))
	 (foo (when (eof-object? sexp) (newline) (exit)))
	 (res (guard (obj (else (display "Caught error:")
				(display (error-object-type obj)) (newline)
				(display (error-object-message obj)) (newline)
				(display (error-object-irritants obj)) (newline)
				#f))
		(eval sexp (interaction-environment)))))
    (guard (obj (else #f))
      (write res))
    (newline)
    (repl)))

(define (compile-file)
  (let ((output (string-append exe-file ".ll")))
    (display (format "compiling ~a to ~a\n" exe-file output-file))
    (flush-output-port)
    (with-output-to-file output (lambda ()
				  (compile exe-file #f include-eval)))
    (link output output-file cc-opts)))

;; Set output file as input file minus extension.
(when (and exe-file (not output-file))
  (let ((no-extension (memq #\. (reverse (string->list exe-file)))))
    (set! output-file
	  (if no-extension
	      (list->string (reverse
			     (cdr no-extension)))
	      "a.out"))))

;; Ok, actually use the options.
(cond
 (exe-file
  (when (file-exists? output-file)
    (delete-file output-file))
  (compile-file))
 (script-file => load)
 (else
  (print-version)
  (repl)))
