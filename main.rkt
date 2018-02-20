#lang racket

(provide #%top-interaction #%app #%top
         (rename-out [my-datum #%datum]
                     [my-module-begin #%module-begin]))
(require (for-syntax syntax/parse racket))

(define-syntax (module-rec stx)
  (syntax-parse stx
    #:datum-literals (=)
    [(_ (id ...) (= x:id y) rest ...)
     #'(let ([x (λ args (apply y args))])
         ; eta expansion here so that `x` attaches to the lambda
         (module-rec (id ... x) rest ...))]
    [(_ (id ...) x rest ...)
     #`(begin (printf "~a: ~a\n" #,(syntax->datum #''x) x)
              (module-rec (id ...) rest ...))]
    [(_ (id ...)) #'(values id ...)]))

(define-syntax (module-helper stx)
  (define (module-collect-id stx)
    (syntax-parse stx
      #:datum-literals (=)
      [((= x:id _) rest ...) (cons #'x (module-collect-id  #'(rest ...)))]
      [(_ rest ...) (module-collect-id #'(rest ...))]
      [() '()]))
  (syntax-parse stx
    [(_ (first-imp rest-imp ...) (prog ...))
     (define lits (module-collect-id #'(prog ...)))
     (define dup (check-duplicates
       (append (syntax->list #'(rest-imp ...)) lits)
       #:key syntax->datum))
     (when dup (raise-syntax-error #f "duplicate definition" dup))
     (with-syntax ([(lits ...) lits])
       #`(#%module-begin
          (require (only-in first-imp rest-imp ...))
          (define-values (lits ...)
            (let-syntax ([lits (λ (stx) (raise-syntax-error #f "unbound id" stx))] ...)
              (module-rec () prog ...)))))]))

(define-syntax (my-module-begin stx)
  (syntax-parse stx
    #:datum-literals (import)
    [(_ (import xs ...) prog ...)
     #'(module-helper (xs ...) (prog ...))]
    [(_ prog ...)
     #'(module-helper (recursive-language/constructs) (prog ...))]))

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
