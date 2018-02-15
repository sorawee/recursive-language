#lang racket

(provide #%top-interaction
         #%app
         #%top
         (rename-out [my-datum #%datum]
                     [my-module-begin #%module-begin]))
(require (for-syntax syntax/parse racket))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Racket forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (module-rec stx)
  (syntax-parse stx
    #:datum-literals (=)
    [(_ (id ...) (= x y) rest ...)
     #'(let ([x (λ args (apply y args))])
         ; eta expansion here so that the name `x` attaches to the function val
         (module-rec (id ... x) rest ...))]
    [(_ (id ...) x rest ...)
     #`(begin (printf "~a: ~a\n" #,(syntax->datum #''x) x)
              (module-rec (id ...) rest ...))]
    [(_ (id ...))
     #'(values id ...)]))

(define-syntax (module-helper stx)
  (define (module-collect-id stx)
    (syntax-parse stx
      #:datum-literals (=)
      [((= x _) rest ...) (cons #'x (module-collect-id  #'(rest ...)))]
      [(_ rest ...) (module-collect-id #'(rest ...))]
      [() '()]))
  (define (check-rec bound stx)
    (syntax-parse stx
      #:datum-literals (=)
      [((= x val) rest ...)
       (check-rec bound #'val)
       (check-rec (cons (syntax->datum #'x) bound) #'(rest ...))]
      [((args ...) rest ...)
       (map (curry check-rec bound) (syntax->list #'(args ...)))
       (check-rec bound #'(rest ...))]
      [() #f]
      [(args ...) (map (curry check-rec bound) (syntax->list stx))]
      [_
       (define e (syntax-e stx))
       (cond
         [(identifier? stx)
          (when (not (member e bound))
            (raise-syntax-error #f "unbound identifier" stx))]
         [(number? e) #f]
         [else #f])]))
  (syntax-parse stx
    [(_ (first-imp rest-imp ...) (prog ...))
     (define ret-stx #'(module-rec () prog ...))
     (define lits (module-collect-id #'(prog ...)))
     (define dup (check-duplicates lits #:key syntax->datum))
     (when dup (raise-syntax-error #f "duplicate definition" dup))
     (check-rec (syntax->datum #'(rest-imp ...)) #'(prog ...))
     #`(#%module-begin
        (require (only-in first-imp rest-imp ...))
        (define-values #,lits #,ret-stx))]))

(define-syntax (my-module-begin stx)
  (syntax-parse stx
    #:datum-literals (import)
    [(_ (import xs ...) prog ...)
     #'(module-helper (xs ...) (prog ...))]
    [(_ prog ...)
     #'(module-helper () (prog ...))]))

(define-syntax (my-datum stx)
  (syntax-parse stx
    [(_ . x)
     (if (regexp-match #px"\\d+" (~v (syntax->datum #'x)))
         #'(#%datum . x)
         (raise-syntax-error #f "value type not supported" #'x))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module reader racket
  (require syntax/strip-context)
  (require "parser.rkt")
  (require "colorer.rkt")
  (provide (rename-out [my-read-syntax read-syntax]
                       [my-read read])
           get-info)
  (define (my-read in) (syntax->datum (my-read-syntax #f in)))
  (define (my-read-syntax src in)
    (strip-context (parse src in "recursive-language")))

  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(color-lexer) do-color]
        [else default]))
    handle-query))
