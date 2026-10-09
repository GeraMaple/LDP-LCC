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
   
   Expresión      not(zero?(0))     not(zero?(1))    not(true)
   Resultado      (boolval #f)      (boolval #t)     (boolval #f)

   and
   ---
   4. Contraste

   Expresión:     and(true, true)     and(true, false)     and(false, car(emptylist))
   Resultao:      (boolval #t)         (boolval #f)        (boolval #f)
   
   or
   --
   1. Propuesta informal
   La disyunción evalúa la primera expresión, si su valor es verdadero, el constructo regresa verdadero sin evaluar la segunda expresión.
   Si es falsa, evalúa la segunda expresión y regresa su valor de verdad.

   2. Especificación formal
   (value-of e1 ρ) = #t
   (value-of (or-exp e1 e2) p) = #t
   
   4. Contraste

   Expresión:      or(false, false)        or(false, true)       or(true, car(emptylist))                                  
   Resultado:      (boolval #f)            (boolval #t)           (boolval #t)                                 


   xor
   ---
   1. Propuesta informal
   La disyunción exclusiva evalúa dos expresiones, devuelve verdadero si y solo si los valores de ambas expresiones son diferentes (una verdadera y una falsa).
   Si ambas son verdaderas o ambas falsas, devuelve falso.

   2. Especificación formal
   xor(e1 e2) -> if e1 the not(e2) else e2

   4. Contraste

   Expresion:       xor(true, false)       xor(true, true)          xor(false, false)                                            
   Resultado:       (boolval #t)           (boolval #f)             (boolval #f)                                                     


   cond
   ----
   2. Especificación formal: solo la regla de cond end

   cond end -> car(emptylist)

   4. Contraste

   Expresion:      cond zero?(0) ==> 10 end           let x=3 in cond zero?(x) ==> 100 zero?(-(x,3)) ==> 200 end             cond zero?(1) ==> 100 end                                        
   Resultao:       (intval 10)                         (intval 200)                                                           car: contract violation    expected: pair?     given: '()

   list
   ----
   4. Contraste

   Expresion:       list(1, 2)                                    let x=4 in list(x, -(x,1), -(x,3))                            list()    
   Resultado:     (listval (list (intval 1) (intval 2)))         (listval (list (intval 4) (intval 3) (intval 1)))             (listval '())                              


   unpack
   ------
   2. Especificación formal

   4. Contraste

   Expresion:       unpack x = list(10) in -(x, 5)             let u=7 in unpack x y = list(u, 3) in -(x,y)              unpack x y = list(1) in x                                                                                 
   Resultado:       (intval 5)                                 (intval 4)                                                value-of: unpack: longitudes no coinciden

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
    [(not-exp e1)
     (define v (value-of e1 ρ))
      (if (val->bool v)
       (boolval #f)
       (boolval #t))]

    [(unpack-exp xs e1 e2)
     (define lst-val (value-of e1 ρ))
     (define lst (val->list lst-val))
     (if (= (length xs) (length lst))
         (value-of e2 (extend-env-varios xs lst ρ))
         (error 'value-of "unpack: longitudes no coinciden"))]

    ;; La tarea, lo que viene hecho en el núcleo: or
    [(or-exp e1 e2)
     (define v1 (value-of e1 ρ))
     (if (val->bool v1)
         (boolval #t)
         (boolval (val->bool (value-of e2 ρ))))]))

;; Si una cláusula necesita una función auxiliar, va aquí abajo, con nombre
;; propio, y no escondida dentro de la cláusula.

;; --- AQUÍ van tus auxiliares, cada una con nombre propio ---
(define (extend-env-varios xs vs env)
  (if (null? xs)
      env
      (extend-env (car xs) (car vs) (extend-env-varios (cdr xs) (cdr vs) env))))

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
