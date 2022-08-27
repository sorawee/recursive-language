#lang racket/base

(provide parse)

(require parser-tools/yacc
         parser-tools/lex
         syntax/parse/define
         "lexer.rkt"
         "error.rkt"
         (for-syntax racket/base
                     racket/syntax))

(define some-original-syntax (read-syntax #f (open-input-string "()")))

(begin-for-syntax
  (define (@/proc stx x start-id end-id)
    (with-syntax ([start-pos (datum->syntax stx (format-symbol
                                                 "~a-start-pos"
                                                 (syntax-e start-id)))]
                  [end-pos (datum->syntax stx (format-symbol
                                               "~a-end-pos"
                                               (syntax-e end-id)))]
                  [src (datum->syntax stx 'src)]
                  [x x])
      #'(datum->syntax #f
                       x
                       (list src
                             (position-line start-pos)
                             (position-col start-pos)
                             (position-offset start-pos)
                             (- (position-offset end-pos)
                                (position-offset start-pos)))
                       some-original-syntax))))

(define-syntax-parser @
  [(_ x) (@/proc this-syntax #'x #'$1 #'$n)]
  [(_ x pos:id) (@/proc this-syntax #'x #'pos #'pos)]
  [(_ x pos-start:id pos-stop:id)
   (@/proc this-syntax #'x #'pos-start #'pos-stop)])

(define (parse src in)
  (define (error-proc tok-ok? tok-name tok-value start-pos end-pos)
    (raise-with-srcloc "syntax error"
                       (srcloc src
                               (position-line start-pos)
                               (position-col start-pos)
                               (position-offset start-pos)
                               (- (position-offset end-pos)
                                  (position-offset start-pos)))))

  (define do-parse
    (parser
     [start stmts]
     [end EOF]
     [tokens basic-tokens empty-tokens]
     [error error-proc]
     [src-pos]
     [grammar
      [stmts [() '()]
             [(stmt stmts) (cons $1 $2)]]
      [stmt [(IMPORT import-ids+ SEMI)
             (@ (list (@ 'import $1) $2 (@ 'recursive-language/construct)))]
            [(ident EQ expr SEMI) (@ (list (@ 'definition $2) $1 $3))]
            [(PRINT display-expr SEMI)
             (@ (list (@ 'print $1) $2))]
            [(CHECK display-expr EQ display-expr SEMI)
             (@ (list (@ 'check $1) $2 $4))]]
      [import-ids+ [(ident) (list $1)]
                   [(ident COMMA import-ids+) (cons $1 $3)]]
      [ident [(IDENT) (@ $1)]]
      [expr [(ident) $1]
            [(ID) #`(#,(@ 'id) #,@$1)]
            [(CONST) #`(#,(@ 'const) #,$1)]
            [(ident LBRACKET exprs RBRACKET) (@ (list* #'macro-call $1 $3))]]
      [exprs [() '()]
             [(expr) (list $1)]
             [(expr COMMA exprs+) (cons $1 $3)]]
      [exprs+ [(expr) (list $1)]
              [(expr COMMA exprs+) (cons $1 $3)]]
      [display-expr [(NUM) (@ $1)]
                    [(expr LPAREN display-exprs RPAREN)
                     (@ (list* #'#%app $1 $3))]]
      [display-exprs [() '()]
                     [(display-expr) (list $1)]
                     [(display-expr COMMA display-exprs+) (cons $1 $3)]]
      [display-exprs+ [(display-expr) (list $1)]
                      [(display-expr COMMA display-exprs+) (cons $1 $3)]]]))

  (do-parse (make-tokenizer src in)))
