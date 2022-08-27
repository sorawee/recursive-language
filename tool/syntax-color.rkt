#lang racket/base

(provide get-syntax-token)

(require parser-tools/lex
         syntax/parse/define
         (prefix-in : parser-tools/lex-sre)
         (for-syntax racket/base))

(define-syntax-parse-rule (return type paren-shape)
  #:with start-pos (datum->syntax this-syntax 'start-pos)
  #:with end-pos (datum->syntax this-syntax 'end-pos)
  #:with lexeme (datum->syntax this-syntax 'lexeme)
  (values lexeme type paren-shape (position-offset start-pos) (position-offset end-pos)))

(define the-lexer
  (lexer
   [(eof) (return 'eof #f)]
   [(:: "#" (:* (:~ "\n"))) (return 'comment #f)]
   [whitespace (return 'white-space #f)]
   ["import" (return 'keyword #f)]
   ["print" (return 'keyword #f)]
   ["check" (return 'keyword #f)]
   ["(" (return 'parenthesis '|(|)]
   [")" (return 'parenthesis '|)|)]
   ["[" (return 'parenthesis '|[|)]
   ["]" (return 'parenthesis '|]|)]
   ["=" (return 'symbol #f)]
   [";" (return 'symbol #f)]
   ["," (return 'symbol #f)]

   [(:: "id_" (:+ numeric) "^" (:+ numeric)) (return 'other #f)]
   [(:: "const_" (:+ numeric)) (return 'other #f)]

   [(:+ numeric) (return 'constant #f)]

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
    (return 'other #f)]))

(define (get-syntax-token port)
  (the-lexer port))
