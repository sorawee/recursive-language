#lang racket

(require syntax-color/racket-lexer
         parser-tools/lex)

(define my-lexer
  (lexer
   [";" (values lexeme 'string #f (position-offset start-pos) (position-offset end-pos))]))

(define (do-color port offset racket-coloring-mode?)
  (cond
    [(equal? (peek-string 1 0 port) ";")
     (define-values (str cat paren start end)
       (my-lexer port))
     (values str cat paren start end 0 #f)]
    [else
     (define-values (str cat paren start end)
       (racket-lexer port))
     (values str cat paren start end 0 #t)]))

(provide do-color)
