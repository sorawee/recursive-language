#lang racket

(provide (all-defined-out))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Basic Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (z x) 0)
(define s add1)

(define (id place arity)
   (λ args
     (when (not (= arity (length args)))
       (error 'id
               (format "Got ~a arguments. Expected arity: ~a"
                       (length args)
                       arity)))
     (list-ref args (sub1 place))))

(define (const n) (λ (_) n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Combinations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (Cn stx)
  (syntax-parse stx
    [(_ f gs ...)
     #'(λ args (f (apply gs args) ...))]))

(define-syntax (Pr stx)
  (syntax-parse stx
    [(_ f g)
     #'(letrec
         ([h (λ args
               (match (last args)
                 [0 (apply f (take args (sub1 (length args))))]
                 [x
                  (define args-dec
                    (list-set args
                              (sub1 (length args))
                              (sub1 (last args))))
                  (apply g
                         (append args-dec
                                 (list (apply h args-dec))))]))])
         h)]))

(define-syntax (Mn stx)
  (syntax-parse stx
    [(_ f)
     #'(λ args
         (let loop ([x 0])
           (if (= 0 (apply f (append args (list x))))
               x
               (loop (add1 x)))))]))

(define (sum x y) (+ x y))
(define (prod x y) (* x y))
(define (monus x y) (max (- x y) 0))
(define (sgn x) (if (= x 0) 0 1))
(define (~sgn x) (if (= x 0) 1 0))
(define (pred x) (max (sub1 x) 0))
