#lang racket

;; ===========================================================================
;; Tests unitarios de parser.rkt: de la cadena al árbol
;;
;; Se prueba parse. El árbol esperado se escribe a mano con los constructores
;; de ast.rkt, que es la comparación que da sentido al parser: el árbol que
;; antes se tecleaba es el que ahora sale del texto. Los árboles son
;; #:transparent, así que check-equal? los compara campo por campo.
;;
;; Los errores de sintaxis son exn:fail comunes, lanzados con error desde
;; report-syntax-error, así que se piden por mensaje con una expresión
;; regular. Los paréntesis del mensaje van escapados, porque en una expresión
;; regular un paréntesis agrupa.
;;
;; CÓMO CORRERLO
;;   raco test tests/parser-tests.rkt    desde let-completo/
;; ===========================================================================

(require rackunit
         "../ast.rkt"
         "../parser.rkt")

;; ---------------------------------------------------------------------------
;; Una producción, una prueba, en el orden de la gramática
;; ---------------------------------------------------------------------------

(check-equal? (parse "7")
              (const-exp 7)
              "Number")

(check-equal? (parse "x")
              (var-exp 'x)
              "Identifier")

(check-equal? (parse "-(3, 4)")
              (diff-exp (const-exp 3) (const-exp 4))
              "-(Expression, Expression)")

(check-equal? (parse "zero?(0)")
              (zero?-exp (const-exp 0))
              "zero?(Expression)")

(check-equal? (parse "if zero?(0) then 1 else 2")
              (if-exp (zero?-exp (const-exp 0)) (const-exp 1) (const-exp 2))
              "if Expression then Expression else Expression")

(check-equal? (parse "let x = 7 in x")
              (let-exp 'x (const-exp 7) (var-exp 'x))
              "let Identifier = Expression in Expression")

;; ---------------------------------------------------------------------------
;; La recursión de la gramática
;; ---------------------------------------------------------------------------

(check-equal? (parse "-(-(1, 2), 3)")
              (diff-exp (diff-exp (const-exp 1) (const-exp 2)) (const-exp 3))
              "una resta dentro de una resta")

(check-equal? (parse "let x = zero?(0) in if x then 1 else 2")
              (let-exp 'x
                       (zero?-exp (const-exp 0))
                       (if-exp (var-exp 'x) (const-exp 1) (const-exp 2)))
              "tres producciones distintas anidadas")

;; ---------------------------------------------------------------------------
;; Lo que decidió el lexer y el parser respeta
;; ---------------------------------------------------------------------------

(check-equal? (parse "-5")
              (const-exp -5)
              "el negativo es una constante, no una resta")

(check-equal? (parse "-(x-1, 2)")
              (diff-exp (var-exp 'x-1) (const-exp 2))
              "x-1 es una variable")

(check-equal? (parse "let let-x = 1 in let-x")
              (let-exp 'let-x (const-exp 1) (var-exp 'let-x))
              "un identificador puede empezar como palabra reservada")

(check-equal? (parse "  7  ")
              (const-exp 7)
              "los espacios no llegan al parser")

;; ---------------------------------------------------------------------------
;; Errores de sintaxis, con el token en que se atora
;; ---------------------------------------------------------------------------

(check-exn #rx"el texto se acabó a la mitad de una expresión"
           (lambda () (parse ""))
           "el texto vacío no es una expresión")

(check-exn #rx"el texto se acabó a la mitad de una expresión"
           (lambda () (parse "-(3, 4"))
           "falta cerrar el paréntesis")

(check-exn #rx"no esperaba NUM \\(4\\)"
           (lambda () (parse "3 4"))
           "después de una expresión completa no va otra")

(check-exn #rx"no esperaba IN"
           (lambda () (parse "let x = in 3"))
           "falta la expresión del valor")

(check-exn #rx"no esperaba RPAREN"
           (lambda () (parse "-(3, 4))"))
           "un paréntesis de más")

(check-exn #rx"no esperaba NUM \\(7\\)"
           (lambda () (parse "let 7 = 7 in 7"))
           "el nombre de un let tiene que ser un identificador")

;; ---------------------------------------------------------------------------
;; De quién es cada error, y el contrato de parse
;; ---------------------------------------------------------------------------

(check-exn #rx"lexer: carácter inesperado"
           (lambda () (parse "3 @ 4"))
           "el error del lexer atraviesa parse sin que el parser lo tape")

(check-exn exn:fail:contract?
           (lambda () (parse 7))
           "parse recibe una cadena: 7 y \"7\" no son lo mismo")

;; ---------------------------------------------------------------------------
;; La aritmética: una producción, una prueba
;;
;; El parser construye el árbol de lo que el texto dice. add sale como
;; add-exp, aunque sea azúcar: quien lo traduce es desugar.
;; ---------------------------------------------------------------------------

(check-equal? (parse "minus(5)")
              (minus-exp (const-exp 5))
              "minus(Expression)")

(check-equal? (parse "add(1, 2)")
              (add-exp (const-exp 1) (const-exp 2))
              "add(Expression, Expression)")

(check-equal? (parse "mul(2, 3)")
              (mul-exp (const-exp 2) (const-exp 3))
              "mul(Expression, Expression)")

(check-equal? (parse "quotient(7, 2)")
              (quotient-exp (const-exp 7) (const-exp 2))
              "quotient(Expression, Expression)")

(check-exn #rx"no esperaba RPAREN"
           (lambda () (parse "add(1)"))
           "add lleva exactamente dos expresiones")

;; ---------------------------------------------------------------------------
;; Los booleanos: una producción, una prueba
;; ---------------------------------------------------------------------------

(check-equal? (parse "true")
              (true-exp)
              "true")

(check-equal? (parse "false")
              (false-exp)
              "false")

(check-equal? (parse "equal?(2, 3)")
              (equal?-exp (const-exp 2) (const-exp 3))
              "equal?(Expression, Expression)")

(check-equal? (parse "less?(2, 3)")
              (less?-exp (const-exp 2) (const-exp 3))
              "less?(Expression, Expression)")

(check-equal? (parse "greater?(2, 3)")
              (greater?-exp (const-exp 2) (const-exp 3))
              "greater?(Expression, Expression)")

(check-equal? (parse "let truely = 1 in truely")
              (let-exp 'truely (const-exp 1) (var-exp 'truely))
              "true es palabra reservada, truely es un nombre")

;; ---------------------------------------------------------------------------
;; Las operaciones con cualquier número de argumentos: la lista vacía, de
;; uno, de varios, y la coma colgando
;; ---------------------------------------------------------------------------

(check-equal? (parse "+(1, 2, 3)")
              (plus-exp (list (const-exp 1) (const-exp 2) (const-exp 3)))
              "+(Expressions): una lista de árboles")

(check-equal? (parse "+()")
              (plus-exp '())
              "+ sin argumentos es la lista vacía")

(check-equal? (parse "+(5)")
              (plus-exp (list (const-exp 5)))
              "+ de uno es una lista de uno")

(check-exn #rx"no esperaba RPAREN"
           (lambda () (parse "+(1,)"))
           "+: después de una coma tiene que venir una expresión")

(check-equal? (parse "*(1, 2, 3)")
              (times-exp (list (const-exp 1) (const-exp 2) (const-exp 3)))
              "*(Expressions): una lista de árboles")

(check-equal? (parse "*()")
              (times-exp '())
              "* sin argumentos es la lista vacía")

(check-equal? (parse "*(5)")
              (times-exp (list (const-exp 5)))
              "* de uno es una lista de uno")

(check-exn #rx"no esperaba RPAREN"
           (lambda () (parse "*(1,)"))
           "*: después de una coma tiene que venir una expresión")

;; ---------------------------------------------------------------------------
;; Las listas: una producción, una prueba
;; ---------------------------------------------------------------------------

(check-equal? (parse "emptylist")
              (emptylist-exp)
              "emptylist: un struct sin campos")

(check-equal? (parse "cons(1, emptylist)")
              (cons-exp (const-exp 1) (emptylist-exp))
              "cons(Expression, Expression)")

(check-equal? (parse "car(x)")
              (car-exp (var-exp 'x))
              "car(Expression)")

(check-equal? (parse "cdr(x)")
              (cdr-exp (var-exp 'x))
              "cdr(Expression)")

(check-equal? (parse "null?(x)")
              (null?-exp (var-exp 'x))
              "null?(Expression)")

(check-equal? (parse "cons(1, cons(2, emptylist))")
              (cons-exp (const-exp 1) (cons-exp (const-exp 2) (emptylist-exp)))
              "una lista de dos es un cons dentro de un cons")

(check-exn #rx"no esperaba RPAREN"
           (lambda () (parse "cons(1)"))
           "cons lleva dos expresiones")

;; ---------------------------------------------------------------------------
;; La tarea: una producción, una prueba
;; ---------------------------------------------------------------------------

(check-equal? (parse "not(x)")
              (not-exp (var-exp 'x))
              "not(Expression)")

(check-equal? (parse "and(x, y)")
              (and-exp (var-exp 'x) (var-exp 'y))
              "and(Expression, Expression)")

(check-equal? (parse "or(x, y)")
              (or-exp (var-exp 'x) (var-exp 'y))
              "or(Expression, Expression)")

(check-equal? (parse "xor(x, y)")
              (xor-exp (var-exp 'x) (var-exp 'y))
              "xor(Expression, Expression)")

(check-equal? (parse "cond zero?(x) ==> 1 zero?(y) ==> 2 end")
              (cond-exp (list (zero?-exp (var-exp 'x)) (zero?-exp (var-exp 'y)))
                        (list (const-exp 1) (const-exp 2)))
              "cond: las pruebas en una lista y los resultados en otra")

(check-equal? (parse "list(1, x)")
              (list-exp (list (const-exp 1) (var-exp 'x)))
              "list(Expressions)")

(check-equal? (parse "unpack x y = l in -(x, y)")
              (unpack-exp '(x y) (var-exp 'l) (diff-exp (var-exp 'x) (var-exp 'y)))
              "unpack: los nombres en una lista de símbolos")

;; ---------------------------------------------------------------------------
;; Las listas vacías de la sintaxis
;; ---------------------------------------------------------------------------

(check-equal? (parse "cond end")
              (cond-exp '() '())
              "un cond sin ramas es sintaxis válida")

(check-equal? (parse "list()")
              (list-exp '())
              "list sin argumentos")

(check-equal? (parse "unpack = l in 5")
              (unpack-exp '() (var-exp 'l) (const-exp 5))
              "unpack sin nombres")

(check-exn #rx"no esperaba END"
           (lambda () (parse "cond zero?(x) ==> end"))
           "a una rama de cond no le puede faltar el resultado")
