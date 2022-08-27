#lang racket/base

(require racket/path
         racket/stxparam
         racket/splicing
         syntax/parse/define
         rackunit
         "construct.rkt"
         (for-syntax racket/base
                     "error.rkt"))

(provide #%app
         #%datum
         #%top-interaction
         (rename-out [module-begin #%module-begin])
         #%top
         import
         print
         check
         definition
         macro-call)

(begin-for-syntax
  (define-syntax-class import-statement
    (pattern ({~literal import} _ _))))

(define-syntax-parameter current-expanding-in-module? #f)

(define-syntax-parse-rule (module-begin i:import-statement ... body ...)
  #:do [(define maybe-displaced-import
          (for/or ([b (in-list (attribute body))])
            (syntax-parse b
              [({~literal import} _ _) b]
              [_ #f])))
        (when maybe-displaced-import
          (raise-with-srcloc
           "import must be at the beginning of the file"
           maybe-displaced-import))]

  #:with dummy-ctx this-syntax
  #:with p (datum->syntax this-syntax (list #'provide (list #'all-defined-out)))

  (#%module-begin
   p
   i ...
   (splicing-syntax-parameterize ([current-expanding-in-module? #t])
     (expand-body (body ...) dummy-ctx))))

;; If
;;   y = x;
;;   x = s;
;;
;; is naively expanded to
;;
;;   (define y x)
;;   (define x s)
;;
;; there would be a binding arrow from x to x, but we don't want that.
;;
;; The solution is to add more scope to the set of scopes
;;
;;   (define y^{1} x)
;;   (define x^{1,2} s^{1})
;;
;; Now, x can no longer refers to x^{1,2}.
;;
;; However, a side effect from this transformation is that we can no longer use
;; x (or y) at the REPL. So we further add:
;;
;;   (define y y^{1})
;;   (define x x^{1,2})
;;
;; This does make x refers to our newly added x, but it no longer creates
;; a visible binding arrow. Good enough I think!
;; At run-time, there would be a use-before-definition error for such program.
(define-syntax-parser expand-body
  [(_ () ctx) #'(begin)]
  [(_ ({~and cur ({~and defn {~literal definition}} x v)} tail ...) ctx)
   (define introducer (make-syntax-introducer #t))
   (define var (introducer #'x))
   #`(begin
       #,(datum->syntax #'cur (list #'defn var #'v) #'cur)
       (define #,(datum->syntax #'ctx (syntax-e var)) #,var)
       (expand-body #,(introducer #'(tail ...)) ctx))]
  [(_ (cur tail ...) ctx)
   #'(begin
       cur
       (expand-body (tail ...) ctx))])

(define-syntax-parse-rule (import [x ...] from)
  #:with out
  (datum->syntax
   this-syntax
   (list #'require (append (list #'only-in #'from) (attribute x))))
  out)

(define-syntax-parse-rule (check actual expected)
  #:do [(define from-module? (syntax-parameter-value #'current-expanding-in-module?))]
  #:with out (quasisyntax/loc this-syntax
               (check-equal? actual expected))
  #:with ret
  (cond
    [from-module? #'(module+ test out)]
    [else #'out])

  ret)

(define-syntax-parse-rule (print x)
  #:with sloc-line (syntax-line #'x)
  #:with sloc-col (syntax-column #'x)
  #:with sloc-source (format "~a" (syntax-source #'x))
  #:do [(define from-module? (syntax-parameter-value #'current-expanding-in-module?))]
  #:with out (quasisyntax/loc this-syntax
               (print/proc 'sloc-source 'sloc-line 'sloc-col x '#,from-module?))
  #:with ret
  (cond
    [from-module? #'(module+ main out)]
    [else #'out])

  ret)

(define (print/proc source line col val from-module?)
  (when from-module?
    (printf "> print <~a:~a:~a>;\n" (file-name-from-path source) line col))
  ((current-print) val))

(define-syntax-parse-rule (definition x:id v)
  #:fail-when
  (and (identifier-binding #'x) #'x)
  (format "~a is already defined/imported" (syntax-e #'x))

  (define x v))

(define-syntax-parse-rule (macro-call f:id args ...)
  #:with ret (quasisyntax/loc this-syntax (f args ...))
  #:with out
  (cond
    [(identifier-binding #'f)
     (cond
       [(bracket-syntax-transformer? (syntax-local-value #'f (λ () #f))) #'ret]
       ;; not macro, or non-bracket-syntax macro
       [else (raise-syntax-error #f "expect Cn, Pr, or Mn" #'f)])]
    [else
     ;; let unbound id error do its job
     #'ret])
  out)
