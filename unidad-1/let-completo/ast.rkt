#lang racket

(provide
 (contract-out
  [expression? (-> any/c boolean?)]
  [struct const-exp ([n exact-integer?])]
  [struct var-exp ([x symbol?])]
  [struct diff-exp ([e1 expression?]
                    [e2 expression?])]
  [struct zero?-exp ([e1 expression?])]
  [struct if-exp ([e1 expression?]
                  [e2 expression?]
                  [e3 expression?])]
  [struct let-exp ([x symbol?]
                   [e1 expression?]
                   [e2 expression?])]

  ;; La aritmética: minus, mul y quotient en el núcleo, add como azúcar
  [struct minus-exp ([e1 expression?])]
  [struct add-exp ([e1 expression?]
                   [e2 expression?])]
  [struct mul-exp ([e1 expression?]
                   [e2 expression?])]
  [struct quotient-exp ([e1 expression?]
                        [e2 expression?])]

  ;; Los booleanos: equal? y less? en el núcleo, greater? como azúcar
  [struct equal?-exp ([e1 expression?]
                      [e2 expression?])]
  [struct less?-exp ([e1 expression?]
                     [e2 expression?])]
  [struct greater?-exp ([e1 expression?]
                        [e2 expression?])]
  ;; Los booleanos que vienen hechos: true y false, como azúcar
  [struct true-exp ()]
  [struct false-exp ()]

  ;; Las operaciones con cualquier número de argumentos: + y *
  [struct plus-exp ([es (listof expression?)])]
  [struct times-exp ([es (listof expression?)])]

  ;; Las listas: los cinco en el núcleo
  [struct emptylist-exp ()]
  [struct cons-exp ([e1 expression?]
                    [e2 expression?])]
  [struct car-exp ([e1 expression?])]
  [struct cdr-exp ([e1 expression?])]
  [struct null?-exp ([e1 expression?])]

  ;; --- AQUÍ van los contratos de tus struct: not-exp, and-exp, cond-exp,
  ;;     list-exp y unpack-exp ---

  ;; La tarea, lo que viene hecho: or en el núcleo y xor como azúcar
  [struct or-exp ([e1 expression?]
                  [e2 expression?])]
  [struct xor-exp ([e1 expression?]
                   [e2 expression?])]))

;; Estructuras de los AST del lenguaje LET
(struct const-exp (n) #:transparent)
(struct var-exp (x) #:transparent)
(struct diff-exp (e1 e2) #:transparent)
(struct zero?-exp (e1) #:transparent)
(struct if-exp (e1 e2 e3) #:transparent)
(struct let-exp (x e1 e2) #:transparent)

;; La aritmética: minus, mul y quotient en el núcleo, add como azúcar
(struct minus-exp (e1) #:transparent)
(struct add-exp (e1 e2) #:transparent)
(struct mul-exp (e1 e2) #:transparent)
(struct quotient-exp (e1 e2) #:transparent)

;; Los booleanos: equal? y less? en el núcleo, greater? como azúcar
(struct equal?-exp (e1 e2) #:transparent)
(struct less?-exp (e1 e2) #:transparent)
(struct greater?-exp (e1 e2) #:transparent)
;; Los booleanos que vienen hechos: true y false, como azúcar
(struct true-exp () #:transparent)
(struct false-exp () #:transparent)

;; Las operaciones con cualquier número de argumentos: + y *
(struct plus-exp (es) #:transparent)
(struct times-exp (es) #:transparent)

;; Las listas: los cinco en el núcleo
(struct emptylist-exp () #:transparent)
(struct cons-exp (e1 e2) #:transparent)
(struct car-exp (e1) #:transparent)
(struct cdr-exp (e1) #:transparent)
(struct null?-exp (e1) #:transparent)

;; --- AQUÍ van tus struct: not-exp, and-exp, cond-exp, list-exp y
;;     unpack-exp ---

;; La tarea, lo que viene hecho: or en el núcleo y xor como azúcar
(struct or-exp (e1 e2) #:transparent)
(struct xor-exp (e1 e2) #:transparent)

(define (expression? obj)
  (or (const-exp? obj)
      (var-exp? obj)
      (diff-exp? obj)
      (zero?-exp? obj)
      (if-exp? obj)
      (let-exp? obj)
      ;; La aritmética: minus, mul y quotient en el núcleo, add como azúcar
      (minus-exp? obj)
      (add-exp? obj)
      (mul-exp? obj)
      (quotient-exp? obj)
      ;; Los booleanos: equal? y less? en el núcleo, greater? como azúcar
      (equal?-exp? obj)
      (less?-exp? obj)
      (greater?-exp? obj)
      ;; Los booleanos que vienen hechos: true y false, como azúcar
      (true-exp? obj)
      (false-exp? obj)
      ;; Las operaciones con cualquier número de argumentos: + y *
      (plus-exp? obj)
      (times-exp? obj)
      ;; Las listas: los cinco en el núcleo
      (emptylist-exp? obj)
      (cons-exp? obj)
      (car-exp? obj)
      (cdr-exp? obj)
      (null?-exp? obj)
      ;; --- AQUÍ van los predicados de tus struct: not-exp?, and-exp?,
      ;;     cond-exp?, list-exp? y unpack-exp? ---

      ;; La tarea, lo que viene hecho: or en el núcleo y xor como azúcar
      (or-exp? obj)
      (xor-exp? obj)))
