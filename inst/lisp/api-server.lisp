(in-package :ecocyc)

(require :sock)
(require :pxml-dual)



;;;; :::::::::::::::::::::: XML-Based API Interface :::::::::::::::::::::::::::::

#||

There are two hurdles that the writer of a new *Cyc (as in PerlCyc and
JavaCyc) API interface needs to surmount as of taltman:Mar-4-2012 . 
First, they must write some standard routines that will read/parse and
write s-expressions, and then they must write 
hundreds of functions (or function stubs) to take advantage of the PTools Lisp API in the
foreign language. For example, there needs to be a stub of a function
for "get-slot-value" that will gather the data from the foreign
language, package it into an s-expression, pass it through the socket
to PTools, and then unpack the s-expression reply and pass the result
to the foreign language. Furthermore, the writing of the functions in the
foreign language is error-prone, as the definitions of functions, and
the functions that have been marked as API functions change over time.

The goals of the XML-based API interface are:

* All s-expressions are passed via XML
* Automate the export of names of API functions and their "signature"
  of arguments in an XML document
* Auto-generate documentation for end-users

The first goal alleviates the writer of a new *Cyc of having to write
custom code for parsing and writing s-expressions (which is bound to
be buggy), since all major languages have support for XML parsing.

The second goal frees the writer of a new *Cyc from the drudgery of
generating pages of boilerplate code that can be automatically
generated via the XML API document.

The third goal makes it easier for the API documentation (both for the Lisp API and foreign language APIs like PerlCyc) to stay up-to-date, since it will be auto-generated from the source-code itself.

TODO: 
* XML Schema definition for sXML?
* More sophisticated API fn publishing
||#

;; Taken from:
;; http://cl-user.net/asp/tags/code-snippets

(defun file-forms (path)
  "Sucks up an entire file from PATH into a list of forms (sexprs),
      returning two values: the list of forms and the number of forms read."
  (with-open-file (s path)
    (loop with eof = (list nil)
	for form = (read s nil eof)
	and form-count from 0
	until (eq form eof)
	collect form into forms
	finally (return (values forms form-count)))))

;; For a sexpr that defines the start of a function (or macro, or class, etc.)
;; return the function name and the 'signature' of the arguments:
(defun decapitate-defun (sexpr)
  (when (member (first sexpr)
		'(defmacro defun defclass defgfpop))
    (subseq sexpr 1 3)))

;; Recurse through an s-expression, collecting the function name and argument signature for certain forms:

(defun collect-all-function-definitions-from-sexpr (sexpr)
  (let ((first-sexpr (first sexpr)))
    (cond ((member first-sexpr
		   '(defmacro defun defclass defgfpop))
	   (list (decapitate-defun sexpr)))
	  (t
	   (loop for sub-sexpr in (rest sexpr)
	       when (listp sub-sexpr)
	       append (collect-all-function-definitions-from-sexpr sub-sexpr))))))

;; Collect functions and their signatures from key files:
;; TODO: Make file paths smarter:

(defun api-function-signatures ()
  (let ((file-paths '("/homedir/brg/main-checkout/beta/aic/generic-fp/beta/lisp/core.lisp"
		      "/homedir/brg/main-checkout/beta/aic/pathway-tools/nav/beta/lisp/relationships.lisp")))
    (loop for file in file-paths		     
	append (collect-all-function-definitions-from-sexpr (file-forms file)))))

;; XML

;; 

;; A quick & dirty function to convert a s-expression into an XML string.
;; This should be fine for the majority of API functions, but will
;; definitely need to be extended to support more general
;; serialization (such as for arrays, hashes, and other objects) eventually:

(defun sexpr->sXML (sexpr)
  (cond ((not (listp sexpr))
	 (concatenate 'string
	   "<atom>"
	   (write-to-string sexpr)
	   "</atom>"))
	((listp sexpr)
	 (concatenate 'string
	   "<sexpr>"
	   (loop for element in sexpr
	       collect (sexpr->sXML element) into sub-sexpr-strings
						
	       finally
		 (return (apply #'concatenate
				(cons 'string sub-sexpr-strings))))
	   "</sexpr>"))))

;; The inverse of sexpr->sXML

(defun sXML->sexpr (sXML)
  (sXML->sexpr-main (net.xml.parser:parse-xml sXML)))

(defun sXML->sexpr-main (sXML-lxml)
  (cond ((listp (first sXML-lxml))
	 (sXML->sexpr-main (first sXML-lxml)))
	((eq (first sXML-lxml)
	     '|atom|)
	 ;; I think it is harmless, but this suppresses the other
	 ;; values returned by read-from-string:
	 (first (multiple-value-list (read-from-string (second sXML-lxml)))))
	(t
	 (loop for element in (rest sXML-lxml)
	     collect (sXML->sexpr-main element)))))

;; Putting it all together:
;; Function for receiving XML, parsing it, eval'ing it, and packaging
;; up the result for return to XML:

(defun xml-repl (sXML &key (verbose? t))
  (handler-case
   (sexpr->sXML (deep-object-name (eval (sXML->sexpr sXML))))
   (error ()
      ;; I would include the invalid sXML in the error message, but
      ;; that might break the XML parser on the other end! There is
      ;; probably a function somewhere to escape out all of the
      ;; necessary XML...
	   (when verbose?
	     (format t "Unable to parse XML:~A~%s-expression: ~A~%"
		     sXML
		     (sXML->sexpr sXML)))
	     (format nil 
		     "<error>xml-repl: unable to parse and eval sXML.</error>"))))
	


	 

;; Auto-doc
