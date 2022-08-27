#lang racket/base

(provide raise-with-srcloc)
(require racket/syntax-srcloc
         racket/path)

(struct exn:fail:with-srcloc exn:fail:user (loc)
  #:property prop:exn:srclocs
  (lambda (self) (list (exn:fail:with-srcloc-loc self))))

(define (raise-with-srcloc msg v)
  (define sloc
    (cond
      [(syntax? v) (syntax-srcloc v)]
      [else v]))
  (define src (srcloc-source sloc))
  (raise (exn:fail:with-srcloc
          (format "~a:~a:~a: ~a"
                  (cond
                    [(path-string? src) (file-name-from-path src)]
                    [else src])
                  (srcloc-line sloc)
                  (srcloc-column sloc)
                  msg)
          (current-continuation-marks)
          sloc)))
