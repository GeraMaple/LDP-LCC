#lang racket

;; ===========================================================================
;; Tests de integración de main.rkt: run y repl
;;
;; Cada prueba atraviesa lexer, parser, value-of, entornos y valores juntos,
;; por la interfaz de main.rkt. Si una falla no dice dónde: dice que las
;; piezas no embonan. Por eso son pocas, y por eso los tests unitarios de
;; cada módulo van aparte.
;;
;; El ciclo repl lee del teclado y escribe en la pantalla. Para probarlo sin
;; teclado, with-input-from-string y with-output-to-string redirigen la
;; entrada y la salida del programa a cadenas mientras dura la llamada.
;;
;; CÓMO CORRERLO
;;   raco test tests/main-tests.rkt      desde let-completo/
;;   raco test tests                     todo
;; ===========================================================================

(require rackunit
         "../vals.rkt"
         "../main.rkt")

;; ---------------------------------------------------------------------------
;; run: del texto al valor
;; ---------------------------------------------------------------------------

(check-equal? (run "let x = 7 in -(x, 2)")
              (intval 5)
              "el programa de siempre, de punta a punta")

(check-equal? (run "zero?(0)")
              (boolval #t)
              "un programa puede valer un booleano")

(check-equal? (run "if zero?(-(3, 3)) then 10 else 20")
              (intval 10)
              "condición calculada")

(check-equal? (run "let y = 10 in if zero?(-(y, 10)) then 1 else 0")
              (intval 1)
              "el ejercicio de las notas")

;; El mismo programa que exp1 en interp-tests.rkt, ahora como texto.
(define prog1
  "let z = -420
   in let x = 666
      in -(let x = 42
           in -(z,
                let w = 7
                in -(w, x)),
           x)")

(check-equal? (run prog1)
              (intval -1051)
              "cuatro let anidados, como texto con saltos de línea")

(check-exn #rx"la variable x no está vinculada"
           (lambda () (run "x"))
           "run evalúa en el entorno vacío: nada viene vinculado de fábrica")

;; ---------------------------------------------------------------------------
;; Cada etapa se queja de lo suyo
;;
;; La expresión regular va anclada al inicio con ^ para que la prueba diga
;; quién se quejó, no solo que alguien se quejó.
;; ---------------------------------------------------------------------------

(check-exn #rx"^lexer:"
           (lambda () (run "3 @ 4"))
           "un carácter que no es de LET lo rechaza el lexer")

(check-exn #rx"^parse:"
           (lambda () (run "zero?(3 3"))
           "tokens en un orden que la gramática no admite los rechaza el parser")

(check-exn #rx"^apply-env:"
           (lambda () (run "-(y, 1)"))
           "un nombre sin vinculación lo rechaza el entorno, desde value-of")

(check-exn #rx"^val->int:"
           (lambda () (run "-(3, zero?(0))"))
           "un operando del tipo equivocado lo rechaza el extractor, desde value-of")

(check-exn exn:fail:contract?
           (lambda () (run 7))
           "run recibe una cadena")

;; ---------------------------------------------------------------------------
;; El ciclo del REPL, con la entrada y la salida como cadenas
;; ---------------------------------------------------------------------------

;; Corre una sesión completa del REPL: input es lo que se teclearía y el
;; resultado es todo lo que el REPL imprimió.
(define (session input)
  (with-output-to-string
    (lambda ()
      (with-input-from-string input repl))))

(check-equal? (session "zero?(0)\n")
              "==> (boolval #t)\n==> "
              "lee, evalúa, imprime y vuelve a pedir")

(check-equal? (session "-(1, 2)\nlet x = 7 in x\n")
              "==> (intval -1)\n==> (intval 7)\n==> "
              "el ciclo repite")

(check-equal? (session "3 @ 4\n-(3, 4)\n")
              "==> lexer: carácter inesperado: @\n==> (intval -1)\n==> "
              "un error se imprime y el REPL sigue vivo")

(check-equal? (session "")
              "==> "
              "el fin de la entrada termina el ciclo")

(check-equal? (session "\n-(3, 4)\n")
              "==> "
              "una línea vacía también termina, aunque haya más texto después")

;; ---------------------------------------------------------------------------
;; La aritmética, de punta a punta
;;
;; run pasa el árbol por desugar antes de evaluarlo, así que aquí es donde se
;; ve que add vale lo que tiene que valer.
;; ---------------------------------------------------------------------------

(check-equal? (run "minus(-(7, 2))")
              (intval -5)
              "minus sobre una resta")

(check-equal? (run "add(3, 4)")
              (intval 7)
              "add se traduce a dos restas y vale la suma")

(check-equal? (run "let x = 4 in mul(x, add(x, 1))")
              (intval 20)
              "un add adentro de un mul adentro de un let")

(check-equal? (run "quotient(-7, 2)")
              (intval -3)
              "quotient trunca hacia cero")

(check-exn #rx"^quotient:"
           (lambda () (run "quotient(7, 0)"))
           "dividir entre cero se queja Racket, no LET")

(check-exn #rx"^val->int:"
           (lambda () (run "add(1, zero?(0))"))
           "el error de un add habla de la resta a la que se tradujo")

;; ---------------------------------------------------------------------------
;; Los booleanos, de punta a punta
;; ---------------------------------------------------------------------------

(check-equal? (run "true")
              (boolval #t)
              "true vale el booleano verdadero")

(check-equal? (run "if false then 1 else 2")
              (intval 2)
              "false como condición")

(check-equal? (run "let x = 3 in if equal?(x, 3) then 1 else 0")
              (intval 1)
              "equal? con una variable")

(check-equal? (run "equal?(true, zero?(0))")
              (boolval #t)
              "equal? entre dos booleanos")

(check-equal? (run "equal?(0, false)")
              (boolval #f)
              "un número y un booleano son distintos")

(check-equal? (run "less?(false, add(1, 2))")
              (boolval #t)
              "false es menor que cualquier número")

(check-equal? (run "greater?(true, 1000)")
              (boolval #t)
              "greater? hereda el orden de less?: true es mayor que todo")

(check-equal? (run "let x = 5 in if greater?(x, 3) then -(x, 3) else -(3, x)")
              (intval 2)
              "una comparación como condición")

;; ---------------------------------------------------------------------------
;; + con cualquier número de argumentos, de punta a punta
;; ---------------------------------------------------------------------------

(check-equal? (run "+(1, 2, 3, 4)")
              (intval 10)
              "+ con cuatro partes")

(check-equal? (run "+()")
              (intval 0)
              "la suma de nada es 0")

(check-equal? (run "let x = 4 in +(x, mul(x, 2), 1)")
              (intval 13)
              "+ con una multiplicación adentro")

(check-exn #rx"^val->int:"
           (lambda () (run "+(1, true)"))
           "+: una parte que no es un número")

;; ---------------------------------------------------------------------------
;; * con cualquier número de argumentos, de punta a punta
;; ---------------------------------------------------------------------------

(check-equal? (run "*(1, 2, 3, 4)")
              (intval 24)
              "* con cuatro partes")

(check-equal? (run "*()")
              (intval 1)
              "el producto de nada es 1")

(check-equal? (run "let x = 4 in *(x, add(x, 1), 2)")
              (intval 40)
              "* con una suma adentro")

(check-exn #rx"^val->int:"
           (lambda () (run "*(2, false)"))
           "*: una parte que no es un número")

;; ---------------------------------------------------------------------------
;; Las listas, de punta a punta
;; ---------------------------------------------------------------------------

(check-equal? (run "let x = 4 in cons(x, cons(-(x, 1), emptylist))")
              (listval (list (intval 4) (intval 3)))
              "una lista construida con el valor de una variable")

(check-equal? (run "if null?(cdr(cons(1, emptylist))) then 1 else 0")
              (intval 1)
              "la cola de una lista de uno es la vacía")

(check-exn #rx"^val->list:"
           (lambda () (run "cons(1, 2)"))
           "una cola que no es lista la rechaza el extractor")

(check-exn #rx"^car:"
           (lambda () (run "car(emptylist)"))
           "car de la vacía lo rechaza el car de Racket, no LET")

(check-equal? (session "car(emptylist)\nnull?(emptylist)\n")
              "==> car: contract violation\n  expected: pair?\n  given: '()\n==> (boolval #t)\n==> "
              "el error de Racket sale en tres renglones y el REPL sigue vivo")

(check-equal? (run "cons(+(1, 2), cons(greater?(2, 1), emptylist))")
              (listval (list (intval 3) (boolval #t)))
              "el azúcar adentro de un cons también se traduce")

(check-equal? (run "equal?(cons(1, emptylist), cons(1, emptylist))")
              (boolval #t)
              "equal? compara valores expresados en general, también dos listas")

;; ---------------------------------------------------------------------------
;; La tarea: los casos que el enunciado publica, uno por constructo
;;
;; Los casos de borde no están aquí. Son tuyos: se trazan con tu
;; especificación y se teclean en el REPL.
;; ---------------------------------------------------------------------------

(check-equal? (run "not(zero?(0))")
              (boolval #f)
              "not, el caso del enunciado")

(check-equal? (run "and(true, false)")
              (boolval #f)
              "and, el caso del enunciado")

(check-equal? (run "or(false, true)")
              (boolval #t)
              "or, el caso del enunciado")

(check-equal? (run "xor(true, true)")
              (boolval #f)
              "xor, el caso del enunciado")

(check-equal? (run "let x = 3 in cond zero?(x) ==> 100 zero?(-(x,3)) ==> 200 end")
              (intval 200)
              "cond, el caso del enunciado")

(check-equal? (run "let x = 4 in list(x, -(x,1), -(x,3))")
              (listval (list (intval 4) (intval 3) (intval 1)))
              "list, el caso del enunciado")

(check-equal? (run "let u = 7 in unpack x y = list(u, 3) in -(x, y)")
              (intval 4)
              "unpack, el caso del enunciado")
