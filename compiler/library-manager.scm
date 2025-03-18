;; Library manager.  Usually the runtime itself has a single manager,
;; but if we're building *another* binary, we need to keep track of its
;; loaded / compiled libraries separately.
(define-record-type libman (%make-libman libraries path envs) libman?
		    (libraries libman-libraries libman-libraries-set!)
		    (path libman-path libman-path-set!)
		    (envs libman-envs))

;; TODO: configure option
(define default-install-path ".")
(define library-search-paths (list
			      (string-append default-install-path "/headers")
			      default-install-path))

(define (make-libman path)
  (%make-libman '() (make-parameter path) (make-hash-table eq?)))

(define runtime-man (make-libman "."))

(define (get-installed-libraries man)
  (libman-libraries man))

;; An individual library.
(define-record-type library (make-library name exports imports) library?
  (name library-name)
  (exports library-exports)
  (imports library-imports))

(define install-library!
  (case-lambda
      ((lib) (install-library! lib runtime-man))
      ((lib man)
       (libman-libraries-set!
	man
	(alist-cons (library-name lib) lib (libman-libraries man))))))

(define (lib-name->strings name)
  (define (to-string x)
    (cond
      ((number? x) (number->string x))
      ((symbol? x) (symbol->string x))
      (else (error "Unknown library name piece:" x))))
  (map to-string name))

(define (lib-name->namespace name) (join "-" (lib-name->strings name)))
(define (lib-name->path name) (join "/" (lib-name->strings name)))
(define (add-current-path man path)
  (string-append ((libman-path man)) "/" path))

(define (do-load-library man name expand-library)
  (define str-name (string-append (lib-name->path name) ".sld"))
  ;; Default paths.
  (define (try-load-paths)
    (let loop ((paths library-search-paths))
      (and
       (pair? paths)
       (let ((file-path (string-append (car paths) "/" str-name)))
         (if (file-exists? file-path)
             (car paths)
             (loop (cdr paths)))))))
  
  (cond
   ((assoc name (libman-libraries man)) => cdr)
   ((try-load-paths)
    =>
    (lambda (path)
      ;;(display "Loading library:") (display name) (newline)
      (with-input-from-file
          (string-append path "/" str-name)
        (lambda ()
	  (parameterize
	      (((libman-path man) (join "/" (cons path (drop-right (lib-name->strings name) 1)))))
	    (expand-library (first (read-file))))
          (cond ((assoc name (libman-libraries man)) => cdr) (else #f))))))
   (else (error "Can't find library in search paths: " name library-search-paths))))
