(import (scheme base) (scheme write) (scheme file)
	(scheme repl)
	(scheme eval)
	(scheme process-context)
	(srfi 1)
	(srfi 69)
	(srfi 151)
	(chicken pretty-print))



(include "util.scm")
(include "qq.scm")
(include "match.scm")
(include "format.scm")
(include "library-manager.scm")
(include "expand.scm")
(include "passes.scm")


(display (cdr (command-line)))
(include "bc.scm")

;(for-each (lambda (x) (compile-file x #t)) (cdr (command-line)))


