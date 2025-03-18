(define-library (library-manager)
  (export install-library! make-library lib-name->namespace library-exports do-load-library 
	  library-name library-imports add-current-path get-installed-libraries make-libman runtime-man libman-envs
	  library-search-paths)
  (import (scheme base)
	  (scheme case-lambda)
	  (srfi 69)
	  (scheme file)
	  (only (srfi 1) first second third fourth fifth sixth map-in-order drop-right alist-cons)
	  (scheme write)
	  (format)
	  (util))
  (include "library-manager.scm")
  )
