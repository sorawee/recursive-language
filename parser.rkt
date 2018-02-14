#lang racket

(provide parse)

(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)

(define keywords '(id const z s Pr Cn Mn =))

(define wsp/p (hidden/p (many/p space/p)))

(define-syntax-rule (do/p x ...) (do x ...))

(define (default-stringify lst) (foldl (lambda (x a) (~a a x)) "" lst))

(define nat/np
  (do/p [v <- (many/p digit/p #:min 1)]
        (pure (string->number (default-stringify v)))))

(define nat/p (do/p [n <- nat/np] wsp/p (pure n)))

(define iden/p
  (syntax/p
   (label/p "id"
            (do/p (string/p "id_")
                  [place <- nat/np]
                  (string/p "^")
                  [arity <- nat/np]
                  wsp/p
                  (pure (list 'id place arity))))))

(define const/p
  (syntax/p
   (label/p "const"
          (do/p (string/p "const_")
                [n <- nat/np]
                wsp/p
                (pure (list 'const n))))))

(define id/np
  (syntax/p
   (label/p "identifier"
            (guard/p (do/p [first <- letter/p]
                           [rest <- (many/p (or/p letter/p digit/p (char-in/p "-~!@#$%^&*_+:./?<>")))]
                           (pure (string->symbol (default-stringify (cons first rest)))))
                     (negate (curryr member keywords))
                     #f
                     (λ (id) (format "Cannot use a keyword as an identifier: ~a" id))))))

(define id/p (do/p [v <- id/np] wsp/p (pure v)))

(define Pr/p
  (syntax/p
   (label/p "Pr"
            (do/p (string/p "Pr")
                  wsp/p
                  (char/p #\[)
                  wsp/p
                  [f <- rec/p]
                  (char/p #\,)
                  wsp/p
                  [g <- rec/p]
                  (string/p "]")
                  wsp/p
                  (pure (list 'Pr f g))))))

(define (comma-separated/p p)
  (syntax/p (many/p p #:sep (do (char/p #\,) wsp/p))))

(define Cn/p
  (syntax/p
   (label/p "Cn"
            (do/p (string/p "Cn")
                  wsp/p
                  (char/p #\[)
                  wsp/p
                  [f <- rec/p]
                  (char/p #\,)
                  wsp/p
                  [args <- (comma-separated/p rec/p)]
                  (char/p #\])
                  wsp/p
                  (pure (list* 'Cn f args))))))

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
         (try/p Pr/p)
         (try/p Cn/p)
         (try/p id/p)
         (id-const/p #\z)
         (id-const/p #\s))))

(define let/p
  (syntax/p
   (label/p "Definition"
            (do/p [x <- id/p]
                  (char/p #\=)
                  wsp/p
                  [val <- rec/p]
                  (char/p #\;)
                  wsp/p
                  (pure (list 'letv x val))))))

(define app/p
  (syntax/p
   (label/p "Call"
            (do/p [f <- rec/p]
                  (char/p #\()
                  wsp/p
                  [args <- (comma-separated/p expr/p)]
                  (char/p #\))
                  wsp/p
                  (char/p #\;)
                  wsp/p
                  (pure (list* f args))))))

(define expr/p
  (syntax/p
   (or/p (try/p nat/p)
         app/p)))

(define statement/p
  (syntax/p
   (or/p (try/p let/p)
         expr/p)))

(define toplevel/p
  (syntax/p
    (do/p wsp/p
      [result <- (many/p statement/p)]
      eof/p
      (pure result))))

(define (parse src in lang)
  (define len (string-length (string-append "#lang " lang)))
  (parse-result!
   (parse-syntax-string toplevel/p
                        (datum->syntax #f (port->string in)
                                       (list src 1 len (add1 len) 0)))))
