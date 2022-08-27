#lang racket/base

(provide make-tokenizer
         basic-tokens
         empty-tokens)

(require racket/match
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens basic-tokens (ID CONST IDENT NUM))
(define-empty-tokens empty-tokens
  (IMPORT PRINT CHECK LPAREN RPAREN COMMA EQ SEMI LBRACKET RBRACKET EOF))

(define (make-tokenizer name port)
  (file-path name)
  (define the-lexer
    (lexer-src-pos
     [(eof) (token-EOF)]
     [(:: "#" (:* (:~ "\n"))) (return-without-pos (the-lexer input-port))]
     [whitespace (return-without-pos (the-lexer input-port))]
     ["import" (token-IMPORT)]
     ["print" (token-PRINT)]
     ["check" (token-CHECK)]
     ["(" (token-LPAREN)]
     [")" (token-RPAREN)]
     ["[" (token-LBRACKET)]
     ["]" (token-RBRACKET)]
     ["=" (token-EQ)]
     [";" (token-SEMI)]
     ["," (token-COMMA)]

     [(:: "id_" (:+ numeric) "^" (:+ numeric))
      (match-let ([(pregexp #px"^id_(\\d+)\\^(\\d+)$" (list _ sub sup)) lexeme])
        (token-ID (list (string->number sub) (string->number sup))))]

     [(:: "const_" (:+ numeric))
      (match-let ([(pregexp #px"^const_(\\d+)$" (list _ sub)) lexeme])
        (token-CONST (string->number sub)))]

     [(:+ numeric) (token-NUM (string->number lexeme))]

     [(:: (:or (char-range #\a #\z)
               (char-range #\A #\Z)
               "."
               "<"
               ">"
               "~"
               "-")
          (:* (:or (char-range #\a #\z)
                   (char-range #\A #\Z)
                   (char-range #\0 #\9)
                   "."
                   "<"
                   ">"
                   "~"
                   "-"
                   "'")))
      (token-IDENT (string->symbol lexeme))]))
  (λ () (the-lexer port)))
