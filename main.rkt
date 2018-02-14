#lang racket

(provide z s
         Cn
         Pr
         id
         const
         #%top-interaction
         #%app
         (rename-out [my-datum #%datum]
                     [my-define define]
                     [my-module-begin #%module-begin]
                     [println print]))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Basic Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (z x) 0)
(define s add1)

(define (id place arity)
   (lambda args
     (when (not (= arity (length args)))
       (error 'id
               (format "Got ~a arguments. Expected arity: ~a"
                       (length args)
                       arity)))
     (list-ref args (sub1 place))))

(define (const n) (lambda (_) n))


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
    #:datum-literals (letv)
    [(_ (letv x y) rest ...)
     #'(let ([x y]) (module-helper rest ...))]
    [(_ x rest ...)
     #`(begin (printf "~a: ~a\n" #,(syntax->datum #''x) x)
              (module-helper rest ...))]
    [(_) #'(void)]))

(require (for-syntax racket))

(define-syntax (my-module-begin stx)
  (syntax-parse stx
    [(_ prog ...) #'(#%module-begin (module-helper prog ...))]))

(define-syntax (my-datum stx)
  (syntax-parse stx
    [(_ . x:nat) #'(#%datum . x)]))

(define-syntax (my-define stx)
  (raise-syntax-error 'define "used out of context" stx))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module reader racket
  (require syntax/strip-context)
  (require "parser.rkt")
  (provide (rename-out [my-read-syntax read-syntax]
                       [my-read read])
           get-info)
  (define (my-read in) (syntax->datum (my-read-syntax #f in)))
  (define (my-read-syntax src in)
    (strip-context
      #`(module src recursive-language
        #,@(parse src in "recursive-language"))))

  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(color-lexer) (dynamic-require 'recursive-language/colorer 'do-color)]
        [else default]))
    handle-query))
