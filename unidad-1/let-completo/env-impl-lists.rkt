#lang racket

(require "vals.rkt")

(provide
 (all-defined-out))

;; Entornos del lenguaje LET
;; Representación lista de asociaciones
;; ((w . 7) (x . 42) (x . 666) (z . -420))

;; ρ = ((x . 42) (x . 666) (z . -420))
;; [w=7]ρ
;; ------------------------------------
;; (cons x v) => (cons 'w 7) => (w . 7)
;; (cons '(w . 7) ρ)
;; =>
;; (cons '(w . 7)
;;       '((x . 42) (x . 666) (z . -420)))
;; =>
;; ((w . 7) (x . 42) (x . 666) (z . -420))

(define (env? obj)
  (and (list? obj)
       (elements-are-bindings? obj)))

(define (elements-are-bindings? lis)
  (or (null? lis)
      (and (pair? (first lis))
           (symbol? (car (first lis)))
           (denval? (cdr (first lis)))
           (elements-are-bindings? (rest lis)))))

(define (empty-env)
  '())

(define (apply-env ρ x)
  (cond
    [(null? ρ)
     (error 'apply-env
            "la variable ~a no está vinculada" x)]
    [(equal? x (car (first ρ)))
     (cdr (first ρ))]
    [else
     (apply-env (rest ρ) x)]))

(define (extend-env x v ρ)
  (cons (cons x v) ρ))
