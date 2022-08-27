#lang info
(define collection "recursive-language")
(define version "2.0")
(define deps '("base"
               "parser-tools-lib"))
(define build-deps '("rackunit-lib"
                     "scribble-lib"
                     "racket-doc"))
(define scribblings '(("scribblings/recursive-language.scrbl" ())))
(define pkg-desc "A language for writing recursively computable functions")
(define pkg-authors '(sorawee))
(define license '(Apache-2.0 OR MIT))
