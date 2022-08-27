#lang racket/base

(require racket/list
         "misc.rkt"
         (for-syntax "misc.rkt"))

(provide z
         s
         id
         Cn
         Pr
         Mn

         const

         sum
         prod
         monus
         sgn
         ~sgn
         pred

         (for-syntax bracket-syntax-transformer?))

(begin-for-syntax
  (define-tagged-struct bracket-syntax-transformer)
  (define-tagged-struct literal-syntax-transformer))

(define-tagged-macro define-bracket-construct bracket-syntax-transformer)
(define-tagged-macro define-literal-construct literal-syntax-transformer)

(define (z x) 0)
(define (s x) (add1 x))

(define-literal-construct (id place arity)
  (λ args
    (when (not (= arity (length args)))
      (apply raise-arity-error 'id arity args))
    (list-ref args (sub1 place))))

(define-literal-construct (const n)
  (λ (_) n))

(define-bracket-construct (Cn f . gs)
  (λ args (apply f (for/list ([g (in-list gs)]) (apply g args)))))

(define-bracket-construct (Pr f g)
  (define (h . args)
    (define larg (last args))
    (case larg
      [(0) (apply f (take args (sub1 (length args))))]
      [else
       (define args-dec (list-set args (sub1 (length args)) (sub1 larg)))
       (apply g (append args-dec (list (apply h args-dec))))]))
  h)

(define-bracket-construct (Mn f)
  (λ args
    (let loop ([x 0])
      (if (zero? (apply f (append args (list x))))
          x
          (loop (add1 x))))))

(define (sum x y) (+ x y))
(define (prod x y) (* x y))
(define (monus x y) (max (- x y) 0))
(define (sgn x) (if (= x 0) 0 1))
(define (~sgn x) (if (= x 0) 1 0))
(define (pred x) (max (sub1 x) 0))
