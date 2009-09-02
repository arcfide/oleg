(library (oleg util)
	(export
		any?
		list-intersperse
		list-intersperse!
		string-rindex
		substring?
		string->integer
		string-split
		make-char-quotator)
	(import
		(rename (rnrs base) (error rnrs-error))
		(rnrs io simple)
		(rnrs mutable-pairs)
		(rnrs unicode)
		(rnrs lists)
		(only (srfi :13) string-index-right string-contains string-null?)
		(oleg prelude)
		(srfi private include))

(define (error msg . irritants)
	(apply rnrs-error `(#f ,msg ,@irritants)))

(include/resolve ("oleg" "ssax" "lib") "util.scm")

)