#lang racket

#|
   TAREA 1. Extender el lenguaje LET

   Nombre:

   Aquí va la prosa de las secciones 1, 2 y 4 de cada constructo, en el
   orden del enunciado. Solo aparecen las secciones que te toca escribir: las
   demás vienen dadas en el enunciado. La sección 3 es el código: tus struct
   en ast.rkt y tus cláusulas en interp.rkt y en desugar.rkt, en los lugares
   marcados.

   not
   ---
   4. Contraste


   and
   ---
   4. Contraste


   or
   --
   1. Propuesta informal

   2. Especificación formal

   4. Contraste


   xor
   ---
   1. Propuesta informal

   2. Especificación formal

   4. Contraste


   cond
   ----
   2. Especificación formal: solo la regla de cond end

   4. Contraste


   list
   ----
   4. Contraste


   unpack
   ------
   2. Especificación formal

   4. Contraste

|#

(require "ast.rkt"
         "env.rkt"
         "vals.rkt")

(provide
 (contract-out
  [value-of (-> expression? env?
                expval?)]))

;; Intérprete del lenguaje LET
(define (value-of e ρ)
  (match e
    [(const-exp n)    (intval n)]
    [(var-exp x)      (apply-env ρ x)]
    [(diff-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (intval (- (val->int v1)
                (val->int v2)))]
    [(zero?-exp e1)
     (define v1 (value-of e1 ρ))
     (boolval (zero? (val->int v1)))]
    [(if-exp e1 e2 e3)
     (define v1 (value-of e1 ρ))
     (if (val->bool v1)
         (value-of e2 ρ)
         (value-of e3 ρ))]
    [(let-exp x e1 e2)
     (define v1 (value-of e1 ρ))
     (define ρ* (extend-env x v1 ρ))
     (value-of e2 ρ*)]

    ;; La aritmética que va en el núcleo: minus, mul y quotient
    [(minus-exp e1)
     (define v1 (value-of e1 ρ))
     (intval (- (val->int v1)))]
    [(mul-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (intval (* (val->int v1)
                (val->int v2)))]
    [(quotient-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (intval (quotient (val->int v1)
                       (val->int v2)))]

    ;; Los booleanos que van en el núcleo: equal? y less?
    [(equal?-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (boolval (equal? v1 v2))]
    [(less?-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (boolval (val<? v1 v2))]

    ;; Las listas
    [(emptylist-exp)
     (listval '())]
    [(cons-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (define v2 (value-of e2 ρ))
     (listval (cons v1 (val->list v2)))]
    [(car-exp e1)
     (define v1 (value-of e1 ρ))
     (car (val->list v1))]
    [(cdr-exp e1)
     (define v1 (value-of e1 ρ))
     (listval (cdr (val->list v1)))]
    [(null?-exp e1)
     (define v1 (value-of e1 ρ))
     (boolval (null? (val->list v1)))]

    ;; --- AQUÍ van tus cláusulas del núcleo: not-exp y unpack-exp ---

    ;; La tarea, lo que viene hecho en el núcleo: or
    [(or-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (if (val->bool v1)
         (boolval #t)
         (boolval (val->bool (value-of e2 ρ))))]))

;; Si una cláusula necesita una función auxiliar, va aquí abajo, con nombre
;; propio, y no escondida dentro de la cláusula.

;; --- AQUÍ van tus auxiliares, cada una con nombre propio ---

;; El orden de los valores expresados: false es menor que todo lo demás,
;; true es mayor que todo lo demás, y entre números es el < de siempre.
;; Es estricto: ningún valor es menor que sí mismo.
(define (val<? v1 v2)
  (match (list v1 v2)
    [(list (boolval #f) (boolval #f)) #f]
    [(list (boolval #f) _)            #t]
    [(list (boolval #t) _)            #f]
    [(list _            (boolval #f)) #f]
    [(list _            (boolval #t)) #t]
    [(list (intval n1)  (intval n2))  (< n1 n2)]))
