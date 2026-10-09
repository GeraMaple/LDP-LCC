#lang racket

;; ===========================================================================
;; Tests unitarios de lexer.rkt: del texto a los tokens
;;
;; Casi todo se prueba con tokenize y los dos selectores token-name y
;; token-value. Los tokens de parser-tools se comparan con equal?, así que
;; dos llamadas a tokenize sobre el mismo texto dan listas iguales.
;;
;; Cada afirmación del README de la X1 tiene aquí su check: los catorce
;; tokens, los valores que cargan, los espacios, las dos reglas de desempate,
;; el error propio del lexer y lo que el lexer no revisa.
;;
;; CÓMO CORRERLO
;;   raco test tests/lexer-tests.rkt     desde let-completo/
;; ===========================================================================

(require rackunit
         "../lexer.rkt")

;; Los nombres de los tokens de una cadena, que es lo que casi siempre se
;; quiere comparar.
(define (names str)
  (map token-name (tokenize str)))

;; El valor del primer token de una cadena.
(define (first-value str)
  (token-value (first (tokenize str))))

;; ---------------------------------------------------------------------------
;; Los catorce tokens, con dos cadenas
;; ---------------------------------------------------------------------------

(check-equal? (names "let x = 7 in -(x, 2)")
              '(LET ID EQ NUM IN MINUS LPAREN ID COMMA NUM RPAREN EOF)
              "diez tokens distintos en el programa de siempre")

(check-equal? (names "if zero?(x) then 1 else 2")
              '(IF ZERO? LPAREN ID RPAREN THEN NUM ELSE NUM EOF)
              "los cuatro que faltaban")

;; ---------------------------------------------------------------------------
;; Los valores que cargan los tokens
;; ---------------------------------------------------------------------------

(check-equal? (first-value "7") 7
              "el lexema ya viene convertido a entero, no es la cadena")

(check-equal? (first-value "x") 'x
              "el nombre viene como símbolo")

(check-equal? (first-value "let") #f
              "un token sin valor no carga nada")

;; ---------------------------------------------------------------------------
;; Espacios y texto vacío
;; ---------------------------------------------------------------------------

(check-equal? (tokenize "  7\n") (tokenize "7")
              "los espacios no producen tokens")

(check-equal? (names "") '(EOF)
              "el texto vacío es solo EOF")

;; ---------------------------------------------------------------------------
;; Las dos reglas de desempate
;;
;; [1] Gana el lexema más largo.
;; [2] A igual longitud, gana la regla que está más arriba.
;; ---------------------------------------------------------------------------

(check-equal? (names "-5") '(NUM EOF)
              "[1] el guion entra al número: un solo token")

(check-equal? (first-value "-5") -5
              "[1] y el valor es el entero negativo")

(check-equal? (names "-(") '(MINUS LPAREN EOF)
              "sin dígito después, el guion es MINUS")

(check-equal? (names "--5") '(MINUS NUM EOF)
              "solo un guion cabe en el número")

(check-equal? (first-value "x-1") 'x-1
              "[1] x-1 es un identificador, no una resta")

(check-equal? (names "zero?") '(ZERO? EOF)
              "[2] a igual longitud gana la palabra reservada, que está arriba")

(check-equal? (first-value "zero?y") 'zero?y
              "[1] más largo le gana a la palabra reservada")

;; ---------------------------------------------------------------------------
;; Errores, y lo que no es error
;; ---------------------------------------------------------------------------

(check-exn #rx"carácter inesperado: @"
           (lambda () (tokenize "3 @ 4"))
           "el error propio del lexer dice el carácter")

(check-exn #rx"carácter inesperado"
           (lambda () (tokenize "3.5"))
           "LET no tiene flotantes: falla en el punto")

(check-equal? (names "zero?(3 3") '(ZERO? LPAREN NUM NUM EOF)
              "pasa sin queja: el orden de los tokens es asunto del parser")

;; ---------------------------------------------------------------------------
;; El predicado
;; ---------------------------------------------------------------------------

(check-true (andmap let-token? (tokenize "let x = 7 in x"))
            "todo lo que sale del lexer es un token de LET")

(check-false (let-token? 7)
             "un entero no es un token")

;; ---------------------------------------------------------------------------
;; La aritmética: cuatro palabras reservadas más
;; ---------------------------------------------------------------------------

(check-equal? (names "minus(1) add(1, 2) mul(1, 2) quotient(1, 2)")
              '(MINUS-KW LPAREN NUM RPAREN
                ADD LPAREN NUM COMMA NUM RPAREN
                MUL LPAREN NUM COMMA NUM RPAREN
                QUOTIENT LPAREN NUM COMMA NUM RPAREN EOF)
              "las cuatro palabras de la aritmética")

(check-equal? (names "minus(-3)") '(MINUS-KW LPAREN NUM RPAREN EOF)
              "la palabra minus y el signo - son tokens distintos")

(check-equal? (first-value "minus-one") 'minus-one
              "[1] más largo le gana a la palabra reservada, también aquí")

;; ---------------------------------------------------------------------------
;; Los booleanos: cinco palabras reservadas más
;; ---------------------------------------------------------------------------

(check-equal? (names "true false") '(TRUE FALSE EOF)
              "las dos constantes")

(check-equal? (names "equal?(1, 2) less?(1, 2) greater?(1, 2)")
              '(EQUAL? LPAREN NUM COMMA NUM RPAREN
                LESS? LPAREN NUM COMMA NUM RPAREN
                GREATER? LPAREN NUM COMMA NUM RPAREN EOF)
              "las tres comparaciones")

(check-equal? (first-value "true?") 'true?
              "[1] más largo le gana a la palabra reservada: true? es un nombre")

;; ---------------------------------------------------------------------------
;; Los signos de las operaciones con cualquier número de argumentos
;; ---------------------------------------------------------------------------

(check-equal? (names "+(1, 2, 3)")
              '(PLUS LPAREN NUM COMMA NUM COMMA NUM RPAREN EOF)
              "el signo + es un token de un carácter")

(check-equal? (names "*(1, 2, 3)")
              '(TIMES LPAREN NUM COMMA NUM COMMA NUM RPAREN EOF)
              "el signo * es un token de un carácter")

;; ---------------------------------------------------------------------------
;; Las listas: cinco palabras reservadas más
;; ---------------------------------------------------------------------------

(check-equal? (names "cons(1, emptylist)")
              '(CONS LPAREN NUM COMMA EMPTYLIST RPAREN EOF)
              "cons y emptylist")

(check-equal? (names "car(x) cdr(x) null?(x)")
              '(CAR LPAREN ID RPAREN CDR LPAREN ID RPAREN NULL? LPAREN ID RPAREN EOF)
              "car, cdr y null?")

(check-equal? (first-value "cons-cell") 'cons-cell
              "[1] más largo le gana a la palabra reservada, también aquí")

;; ---------------------------------------------------------------------------
;; La tarea: ocho palabras reservadas y la flecha
;; ---------------------------------------------------------------------------

(check-equal? (names "not(x) and(x, y) or(x, y) xor(x, y)")
              '(NOT LPAREN ID RPAREN
                AND LPAREN ID COMMA ID RPAREN
                OR LPAREN ID COMMA ID RPAREN
                XOR LPAREN ID COMMA ID RPAREN EOF)
              "los cuatro booleanos")

(check-equal? (names "cond zero?(x) ==> 1 end")
              '(COND ZERO? LPAREN ID RPAREN ARROW NUM END EOF)
              "cond, la flecha y end")

(check-equal? (names "unpack x y = list(1, 2) in x")
              '(UNPACK ID ID EQ LIST LPAREN NUM COMMA NUM RPAREN IN ID EOF)
              "unpack y list")

(check-equal? (names "= ==> =") '(EQ ARROW EQ EOF)
              "[1] la flecha le gana al signo igual por ser más larga")

(check-equal? (first-value "android") 'android
              "[1] más largo le gana a la palabra reservada: android es un nombre")
