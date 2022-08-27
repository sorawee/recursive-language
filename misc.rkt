#lang racket/base

(provide define-tagged-struct
         define-tagged-macro)
(require racket/match
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parse-rule (define-tagged-struct tag)
  (struct tag (trans)
    #:property prop:procedure
    (λ (self stx)
      (match-define (tag trans) self)
      (trans stx))))

(define-syntax-parse-rule (define-tagged-macro define- struct-)
  #:with ooo (quote-syntax ...)
  (define-syntax-parse-rule (define- (name . args) body ...+)
    (begin
      (define-syntax name
        (struct-
         (λ (stx)
           (syntax-parse stx
             [(_ . args) #'(name/proc . args)]
             [_ (raise-syntax-error #f "incorrect use" stx)]))))
      (define (name/proc . args) body ooo))))
