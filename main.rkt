#lang racket

(provide z s
         Cn
         Pr
         #%top-interaction
         #%app
         (rename-out [my-datum #%datum]
                     [my-define define]
                     [my-top #%top]
                     [my-module-begin #%module-begin]
                     [println print]))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Basic Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (z x) 0)
(define s add1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Combinations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (Cn stx)
  (syntax-parse stx
    [(_ f gs ...)
     #'(λ args (f (apply gs args) ...))]))

(define-syntax (Pr stx)
  (syntax-parse stx
    [(_ f g)
     #'(letrec
           ([h (λ args
                 (match (last args)
                   [0 (apply f (take args (sub1 (length args))))]
                   [x
                    (define args-dec
                      (list-set args
                                (sub1 (length args))
                                (sub1 (last args))))
                    (apply g
                           (append args-dec
                                   (list (apply h args-dec))))]))])
         h)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Racket forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (module-helper stx)
  (syntax-parse stx
    #:datum-literals (define)
    [(_ (define x y) rest ...)
     #'(let ([x y]) (module-helper rest ...))]
    [(_ x rest ...)
     #`(begin (printf "~a: ~a\n" #,(syntax->datum #''x) x)
              (module-helper rest ...))]
    [(_) #'(void)]))

(define-syntax (my-module-begin stx)
  (syntax-parse stx
    [(_ prog ...) #'(#%module-begin (module-helper prog ...))]))

(define-syntax (my-datum stx)
  (syntax-parse stx
    [(_ . x:nat) #'(#%datum . x)]))

(define-syntax (my-define stx)
  (raise-syntax-error 'define "used out of context" stx))

(define-syntax-rule (my-top . x) (special-fun (symbol->string 'x)))

(define (special-fun s)
  (match (regexp-match #px"^id_(\\d+)\\^(\\d+)$" s)
    [(list _ place arity)
     (lambda args
       (when (not (= (string->number arity) (length args)))
         (error 'id
                "Got ~a arguments. Expected arity: ~a"
                (length args)
                (string->number arity)))
       (list-ref args (sub1 (string->number place))))]
    [#f (match (regexp-match #px"^const_(\\d+)$" s)
          [(list _ n) (lambda (_) (string->number n))]
          [#f (error (string->symbol s) "unbound identifier")])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module reader syntax/module-reader
  recursive-language)