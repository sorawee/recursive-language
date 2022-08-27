#lang racket/base

(provide configure)

(require "../parser.rkt"
         syntax/strip-context)

(define (configure data)
  (current-read-interaction the-read))

(define (the-read src ip)
  (cond
    [(or (not (char-ready? ip)) (eof-object? (peek-char ip))) eof]
    [else #`(begin #,@(strip-context (parse src ip)))]))
