(define-library
  (util)
  (export imap read-file ilength omap ofilter for ilist? to-proper join gen-sym push! inc! pop! hash-table-empty? while until string-join for-each-pair bytevector->list)
  (import (scheme base) (scheme case-lambda) (scheme char) (only (srfi 1) filter fold)  (srfi 69) (scheme read) (scheme complex))
  (include "util.scm"))
