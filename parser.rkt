#lang racket

(provide parse)

(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)
(require (for-syntax syntax/parse))

(define-syntax-rule (do/p x ...) (do x ...))
(define-syntax (wrap/p stx)
  (syntax-parse stx
    #:datum-literals (<? ?>)
    [(_ pre ... <? ret ?> post ...)
     #'(do/p pre ... [v <- ret] post ... (pure v))]))

(define (default-stringify lst) (foldl (λ (x a) (~a a x)) "" lst))

(define wsp/p (hidden/p (many/p space/p)))
(define wspr/p (hidden/p (many+/p space/p)))

(define (spacet/p p) (wrap/p <? p ?> wsp/p))

; Do not allow $ as the first character, as it's the access to unhygienic
; variables which could introduce recursion
(define allowed-first-chars "~$")
(define allowed-chars "-~!@#$%^&*_+:./?<>")
(define keywords '(id const z s Pr Cn Mn = import))

(define (symbol/np s)
  (syntax/p (chain (compose1 pure string->symbol) (string/p s))))

(define nat/np
  (chain (compose1 pure string->number default-stringify)
         (many/p digit/p #:min 1)))

(define nat/p (spacet/p nat/np))

(define iden/p
  (syntax/p
   (label/p
    "id"
    (do/p [id <- (symbol/np "id")]
          (char/p #\_)
          [place <- nat/np]
          (char/p #\^)
          [arity <- nat/np]
          wsp/p
          (pure (list id place arity))))))

(define const/p
  (syntax/p
   (label/p
    "const"
    (do/p [const <- (symbol/np "const")]
          (char/p #\_)
          [n <- nat/np]
          wsp/p
          (pure (list const n))))))

(define id/rnp
  (syntax/p
    (label/p
      "identifier"
      (do/p [first <- (or/p letter/p
                            (char-in/p allowed-first-chars))]
            [rest <- (many/p (or/p letter/p
                                   digit/p
                                   (char-in/p allowed-chars)))]
            (pure (string->symbol
                   (default-stringify (cons first rest))))))))

(define id/np
  (guard/p
    id/rnp
    (λ (id) (not (member (syntax->datum id) keywords)))
    #f
    (λ (id) (format "Cannot use a keyword as an identifier: ~a" id))))

(define id/p (spacet/p id/np))
(define id/rp (spacet/p id/rnp))

(define (comma-separated/p p min-val max-val)
  (syntax/p (many/p p #:sep (do (char/p #\,) wsp/p) #:min min-val #:max max-val)))

(define semi/p (hidden/p (do/p (char/p #\;) wsp/p)))

(define import/p
  (syntax/p
   (label/p
    "import"
    (do/p [stx-loc <- (syntax/p (string/p "import"))]
          wspr/p
          [args <- (comma-separated/p id/rp 1 +inf.0)]
          semi/p
          wsp/p
          (pure `(import
            ,(datum->syntax stx-loc 'recursive-language/constructs stx-loc stx-loc)
            ,@args))))))

(define (comb/p name min-val max-val)
  (syntax/p
   (label/p
    name
    (do/p [name <- (try/p (wrap/p <? (symbol/np name) ?>
                                  wsp/p
                                  (char/p #\[)))]
          wsp/p
          [args <- (comma-separated/p rec/p min-val max-val)]
          (char/p #\])
          wsp/p
          (pure (cons name args))))))

(define (id-const/p c)
  (syntax/p
   (do/p
    [v <- (char/p c)]
    wsp/p
    (pure (string->symbol (default-stringify (list v)))))))

(define rec/p
  (syntax/p
   (or/p (try/p iden/p)
         (try/p const/p)
         (comb/p "Pr" 2 2)
         (comb/p "Cn" 2 +inf.0)
         (comb/p "Mn" 1 1)
         (try/p id/p)
         (id-const/p #\z)
         (id-const/p #\s))))

(define let/p
  (syntax/p
   (label/p
    "Definition"
    (do/p [x <- (try/p (wrap/p <? id/p ?> (char/p #\=)))]
          wsp/p
          [val <- rec/p]
          (pure (list '= x val))))))

(define app/p
  (syntax/p
   (label/p
    "Call"
    (do/p [f <- rec/p]
          (char/p #\()
          wsp/p
          [args <- (comma-separated/p expr/p 0 +inf.0)]
          (char/p #\))
          wsp/p
          (pure (list* f args))))))

(define expr/p (syntax/p (or/p nat/p app/p)))
(define statement/p
  (syntax/p (wrap/p <? (or/p let/p expr/p) ?> (char/p #\;) wsp/p)))
(define (maybe/p p) (or/p (try/p p) void/p))

(define (toplevel/p lang-name)
  (syntax/p
   (do/p (string/p "#lang ")
         [lang <- (symbol/np lang-name)]
         wsp/p
         [imports <- (maybe/p import/p)]
         [result <- (many/p statement/p)]
         eof/p
         (define imps (if (void? imports) empty (list imports)))
         (pure `(module ,(string->symbol (string-append lang-name "-mod"))
                 ,lang ,@imps ,@result)))))

(define (parse src in lang-name)
  (parse-result!
   (parse-syntax-string
    (toplevel/p lang-name)
    (datum->syntax
     #f (string-append "#lang " lang-name (port->string in))
     (list src 1 0 1 0)))))
