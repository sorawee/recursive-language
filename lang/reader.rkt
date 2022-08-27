(module reader syntax/module-reader
  #:language 'recursive-language
  #:read read
  #:read-syntax read-syntax
  #:whole-body-readers? #t
  #:language-info '#(recursive-language/lang/lang-info get-info #f)
  #:info (lambda (key defval default)
           (case key
             [(drracket:default-filters) '(["Recursive Lang Sources" "*.rl"])]
             [(drracket:default-extension) "rl"]
             [(drracket:submit-predicate)
              (dynamic-require 'recursive-language/tool/submit 'repl-submit?)]
             [(color-lexer)
              (dynamic-require 'recursive-language/tool/syntax-color 'get-syntax-token)]
             [else (default key defval)]))
  (require syntax/strip-context
           "../parser.rkt")

  (define (read [in (current-input-port)])
    (syntax->datum (read-syntax #f in)))

  (define (read-syntax [src #f] [in (current-input-port)])
    (strip-context (parse src in))))
