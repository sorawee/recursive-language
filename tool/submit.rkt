#lang racket/base

(provide repl-submit?)

(define (repl-submit? ip has-white-space?)
  (let loop ([closed? #f] [in-comment? #f])
    (define c (read-char ip))
    (cond
      [(eof-object? c) closed?]
      [(eqv? c #\newline) (loop closed? #f)]
      [in-comment? (loop closed? #t)]
      [(eqv? c #\#) (loop closed? #t)]
      [(eqv? c #\;) (loop #t #f)]
      [(char-whitespace? c) (loop closed? #f)]
      [else (loop #f #f)])))

(module+ test
  (require rackunit)

  (define (test s)
    (repl-submit? (open-input-string s) #f))

  (check-true (test "print 1;"))
  (check-true (test "print 1; # hello"))
  (check-false (test "print 1 # hello"))
  (check-false (test "print 1 # hello\n"))
  (check-true (test "print 1 # hello\n;"))
  (check-false (test "print 1 # hello;"))
  (check-true (test "print 1 # hello;\n  ;")))
