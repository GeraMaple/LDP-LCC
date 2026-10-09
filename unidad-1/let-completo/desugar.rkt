#lang racket

;; ===========================================================================
;; Traductor de azúcar sintáctica del lenguaje LET
;;
;; No todo constructo nuevo necesita una cláusula en value-of. Algunos se
;; pueden decir con lo que el lenguaje ya tiene: add(e1, e2) es lo mismo que
;; -(e1, -(0, e2)). A un constructo así se le llama AZÚCAR SINTÁCTICA, y a lo
;; que value-of sí sabe evaluar se le llama el NÚCLEO del lenguaje.
;;
;; El intérprete gana una etapa, entre el parser y value-of:
;;
;;   texto --[lexer]--> tokens --[parser]--> árbol --[desugar]--> árbol del núcleo --[value-of]--> valor
;;
;; desugar recibe un árbol que puede traer constructos de azúcar y devuelve
;; un árbol que solo trae constructos del núcleo. La semántica de un
;; constructo de azúcar no es una regla de inferencia: es una REGLA DE
;; TRANSFORMACIÓN, que dice a qué expresión se traduce.
;;
;;   add(e1, e2)  ⇝  -(e1, -(0, e2))
;;
;; Cada regla de transformación es una cláusula de este archivo, igual que
;; cada regla de inferencia es una cláusula de interp.rkt.
;; ===========================================================================

(require "ast.rkt")

(provide
 (contract-out
  [desugar (-> expression? expression?)]))

;; ---------------------------------------------------------------------------
;; desugar recorre el árbol entero, y tiene dos clases de cláusula:
;;
;;   - Las del NÚCLEO reconstruyen el mismo constructo con sus partes
;;     traducidas. El azúcar puede venir adentro de cualquier cosa, por
;;     ejemplo en let x = add(1, 2) in x, así que hay que bajar a todas las
;;     subexpresiones. Las hojas, const-exp y var-exp, no tienen partes y se
;;     devuelven tal cual.
;;
;;   - Las de AZÚCAR construyen la traducción y LA VUELVEN A PASAR POR
;;     desugar. Hace falta por dos razones: las partes e1 y e2 todavía no se
;;     han traducido, y la traducción misma puede estar escrita con otro
;;     constructo de azúcar.
;; ---------------------------------------------------------------------------

(define (desugar e)
  (match e
    ;; El núcleo de LET: se reconstruye igual
    [(const-exp n)     e]
    [(var-exp x)       e]
    [(diff-exp e1 e2)  (diff-exp (desugar e1) (desugar e2))]
    [(zero?-exp e1)    (zero?-exp (desugar e1))]
    [(if-exp e1 e2 e3) (if-exp (desugar e1) (desugar e2) (desugar e3))]
    [(let-exp x e1 e2) (let-exp x (desugar e1) (desugar e2))]

    ;; El núcleo agregado por la aritmética: se reconstruye igual
    [(minus-exp e1)        (minus-exp (desugar e1))]
    [(mul-exp e1 e2)       (mul-exp (desugar e1) (desugar e2))]
    [(quotient-exp e1 e2)  (quotient-exp (desugar e1) (desugar e2))]

    ;; El núcleo agregado por los booleanos: se reconstruye igual
    [(equal?-exp e1 e2)    (equal?-exp (desugar e1) (desugar e2))]
    [(less?-exp e1 e2)     (less?-exp (desugar e1) (desugar e2))]

    ;; El núcleo agregado por las listas: se reconstruye igual
    [(emptylist-exp)       e]
    [(cons-exp e1 e2)      (cons-exp (desugar e1) (desugar e2))]
    [(car-exp e1)          (car-exp (desugar e1))]
    [(cdr-exp e1)          (cdr-exp (desugar e1))]
    [(null?-exp e1)        (null?-exp (desugar e1))]

    ;; El núcleo agregado por la tarea: se reconstruye igual
    [(not-exp e1)          (not-exp (desugar e1))]
    [(or-exp e1 e2)        (or-exp (desugar e1) (desugar e2))]
    [(unpack-exp xs e1 e2) (unpack-exp xs (desugar e1) (desugar e2))]

    ;; El azúcar: se traduce y se vuelve a pasar por desugar
    ;; add(e1, e2) ⇝ -(e1, -(0, e2))
    [(add-exp e1 e2)
     (desugar (diff-exp e1 (diff-exp (const-exp 0) e2)))]
    ;; greater?(e1, e2) ⇝ less?(e2, e1)
    [(greater?-exp e1 e2)
     (desugar (less?-exp e2 e1))]

    ;; El azúcar con cualquier número de argumentos
    [(plus-exp es)
     (cond
       ;; +() ⇝ 0
       [(null? es) (desugar (const-exp 0))]
       ;; +(e, es ...) ⇝ add(e, +(es ...))
       [else       (desugar (add-exp (first es)
                                     (plus-exp (rest es))))])]
    [(times-exp es)
     (cond
       ;; *() ⇝ 1
       [(null? es) (desugar (const-exp 1))]
       ;; *(e, es ...) ⇝ mul(e, *(es ...))
       [else       (desugar (mul-exp (first es)
                                     (times-exp (rest es))))])]

    ;; El azúcar de la tarea
    ;; --- AQUÍ van tus cláusulas de azúcar: and-exp, cond-exp y list-exp ---
    [(and-exp e1 e2)
     (desugar (if-exp e1 e2 (false-exp)))]

    [(cond-exp es1 es2)
     (if (null? es1)
       (desugar (car-exp (emptylist-exp)))
       (desugar (if-exp (car es1) (car es2) (cond-exp (cdr es1) (cdr es2)))))]

    [(list-exp es)
     (if (null? es)
       (desugar (emptylist-exp))
       (desugar (cons-exp (car es) (list-exp (cdr es)))))]

    ;; El azúcar de la tarea que viene hecho: xor
    [(xor-exp e1 e2)
     (desugar (if-exp e1 (not-exp e2) e2))]

    ;; El azúcar que viene hecho: true y false
    [(true-exp)   (desugar (zero?-exp (const-exp 0)))]
    [(false-exp)  (desugar (zero?-exp (const-exp 1)))]))

;; ===========================================================================
;; PARA PROBAR EN EL REPL
;;
;;   > (desugar (add-exp (const-exp 1) (const-exp 2)))
;;   (diff-exp (const-exp 1) (diff-exp (const-exp 0) (const-exp 2)))
;;
;; Un árbol del núcleo sale igual que entró:
;;
;;   > (desugar (diff-exp (const-exp 3) (const-exp 4)))
;;   (diff-exp (const-exp 3) (const-exp 4))
;;
;; Y el azúcar se encuentra aunque esté adentro de un constructo del núcleo:
;;
;;   > (desugar (let-exp 'x (add-exp (const-exp 1) (const-exp 2)) (var-exp 'x)))
;;   (let-exp 'x (diff-exp (const-exp 1) (diff-exp (const-exp 0) (const-exp 2)))
;;            (var-exp 'x))
;; ===========================================================================
