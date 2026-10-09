#lang racket

;; ===========================================================================
;; Parser del lenguaje LET
;;
;; El lexer ya parte el texto en tokens. Falta la segunda etapa:
;;
;;   texto ---[lexer]---> tokens ---[parser]---> árbol ---[value-of]---> valor
;;
;; (Entre el parser y value-of hay ahora una etapa más, desugar. Está contada
;; en desugar.rkt. Al parser no le cambia nada: sigue construyendo el árbol
;; de lo que el texto dice.)
;;
;; El parser recibe los tokens en orden y construye el árbol de sintaxis
;; abstracta, el mismo que hasta ahora tecleábamos a mano con los struct de
;; ast.rkt. De los doce tokens de "let x = 7 in -(x, 2)" sale
;;
;;   (let-exp 'x (const-exp 7) (diff-exp (var-exp 'x) (const-exp 2)))
;;
;; Para saber qué árbol construir, el parser conoce la GRAMÁTICA. Es quien
;; decide que "zero?(3 3" está mal formado, aunque el lexer lo haya dejado
;; pasar: le falta una coma y un paréntesis para ser una resta, y le sobra un
;; número para ser un zero?.
;;
;; Igual que el lexer, el parser no se escribe a mano. Se DESCRIBE con la
;; gramática y la biblioteca parser-tools genera el código que la reconoce.
;; ===========================================================================

(require parser-tools/yacc
         "lexer.rkt"
         "ast.rkt")

;; parser-tools/yacc trae la forma parser. De lexer.rkt vienen next-token, los
;; dos grupos de tokens y token-name. De ast.rkt vienen los struct del árbol: el
;; parser no los vuelve a definir, los construye.

(provide
 (contract-out
  ;; De una cadena al árbol
  [parse (-> string?
             expression?)]

  ;; De un proveedor de tokens al árbol. Es lo que parser-tools genera.
  [parse-tokens (-> (-> let-token?)
                    expression?)]))

;; ---------------------------------------------------------------------------
;; 1. LA GRAMÁTICA, OTRA VEZ
;;
;;   Expression ::= Number                                       const-exp
;;               |  Identifier                                   var-exp
;;               |  -(Expression, Expression)                    diff-exp
;;               |  zero?(Expression)                            zero?-exp
;;               |  if Expression then Expression else Expression   if-exp
;;               |  let Identifier = Expression in Expression    let-exp
;;
;; Seis producciones, seis struct. Y ahora, seis reglas del parser: una por
;; producción, con la misma forma. A la izquierda de cada regla va la
;; secuencia de tokens y no terminales que la producción genera. A la derecha
;; va el struct que construye con ellos.
;;
;; La aritmética agrega cuatro producciones:
;;
;;   Expression ::= minus(Expression)                             minus-exp
;;               |  add(Expression, Expression)                   add-exp
;;               |  mul(Expression, Expression)                   mul-exp
;;               |  quotient(Expression, Expression)              quotient-exp
;;
;; Los booleanos agregan cinco:
;;
;;   Expression ::= true                                          true-exp
;;               |  false                                         false-exp
;;               |  equal?(Expression, Expression)                equal?-exp
;;               |  less?(Expression, Expression)                 less?-exp
;;               |  greater?(Expression, Expression)              greater?-exp
;;
;; Y las operaciones con cualquier número de argumentos:
;;
;;   Expression ::= +(Expressions)                                plus-exp
;;               |  *(Expressions)                                times-exp
;;
;; Reciben cero o más expresiones separadas por comas. Eso no se puede decir
;; con una sola producción: hacen falta dos no terminales para la lista,
;;
;;   Expressions         ::= ε
;;                        |  NonemptyExpressions
;;   NonemptyExpressions ::= Expression
;;                        |  Expression , NonemptyExpressions
;;
;; donde ε es la cadena vacía. Expressions admite que no haya nada entre los
;; paréntesis, y NonemptyExpressions exige una expresión después de cada
;; coma, para que +(1,) con la coma colgando no pase. En el parser de abajo
;; son expressions y nonempty-expressions.
;;
;; Las listas agregan cinco:
;;
;;   Expression ::= emptylist                                     emptylist-exp
;;               |  cons(Expression, Expression)                  cons-exp
;;               |  car(Expression)                               car-exp
;;               |  cdr(Expression)                               cdr-exp
;;               |  null?(Expression)                             null?-exp
;;
;; Y la tarea agrega siete:
;;
;;   Expression ::= not(Expression)                               not-exp
;;               |  and(Expression, Expression)                   and-exp
;;               |  (escribe la producción de or)                 or-exp
;;               |  (escribe la producción de xor)                xor-exp
;;               |  cond {Expression ==> Expression}* end         cond-exp
;;               |  list(Expressions)                             list-exp
;;               |  (escribe la producción de unpack)             unpack-exp
;;
;; La gramática tiene también Program ::= Expression, pero no hay un struct
;; para Program, así que el parser arranca en Expression.
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; 2. LOS ERRORES DE SINTAXIS
;;
;; Cuando llega un token que ninguna regla admite en ese punto, parser-tools
;; llama a la función de error con tres cosas: si el token era válido, su
;; nombre y su valor. Nosotros solo usamos el nombre y el valor.
;;
;; El caso de EOF se dice aparte. "no esperaba EOF" es correcto pero se lee
;; mal: lo que pasó es que el texto se acabó a la mitad de una expresión.
;;
;; Va antes que el parser porque el parser la nombra al construirse.
;; ---------------------------------------------------------------------------

(define (report-syntax-error token-ok? name value)
  (cond
    [(eq? name 'EOF)
     (error 'parse "el texto se acabó a la mitad de una expresión")]
    [value
     (error 'parse "no esperaba ~a (~a)" name value)]
    [else
     (error 'parse "no esperaba ~a" name)]))

;; ---------------------------------------------------------------------------
;; 3. EL PARSER
;;
;; parser recibe cinco cosas:
;;
;;   (start expression)   el no terminal por el que empieza: el símbolo
;;                        inicial de la gramática
;;   (end EOF)            el token que dice que ya no hay más
;;   (tokens ...)         los grupos de tokens que va a recibir, los mismos
;;                        que declaró lexer.rkt
;;   (error ...)          qué hacer cuando llega un token que la gramática
;;                        no permite en ese lugar
;;   (grammar ...)        las reglas, un bloque por no terminal
;;
;; Dentro de una regla, $1, $2, $3, ... son las piezas de la izquierda, en
;; orden. En [(MINUS LPAREN expression COMMA expression RPAREN) ...] la pieza
;; $3 es la primera expression y la $5 es la segunda. Las piezas que son
;; tokens con valor traen el valor: en [(NUM) (const-exp $1)] la pieza $1 es
;; el entero, no el token. Las que son no terminales traen el árbol que ya se
;; construyó para ellas.
;; ---------------------------------------------------------------------------

(define parse-tokens
  (parser
   (start expression)
   (end EOF)
   (tokens value-tokens empty-tokens)
   (error report-syntax-error)

   (grammar
    (expression
     ;; Number
     [(NUM)                                             (const-exp $1)]
     ;; Identifier
     [(ID)                                              (var-exp $1)]
     ;; -(Expression, Expression)
     [(MINUS LPAREN expression COMMA expression RPAREN) (diff-exp $3 $5)]
     ;; zero?(Expression)
     [(ZERO? LPAREN expression RPAREN)                  (zero?-exp $3)]
     ;; if Expression then Expression else Expression
     [(IF expression THEN expression ELSE expression)   (if-exp $2 $4 $6)]
     ;; let Identifier = Expression in Expression
     [(LET ID EQ expression IN expression)              (let-exp $2 $4 $6)]

     ;; Las extensiones de la aritmética
     ;; minus(Expression)
     [(MINUS-KW LPAREN expression RPAREN)                    (minus-exp $3)]
     ;; add(Expression, Expression)
     [(ADD LPAREN expression COMMA expression RPAREN)        (add-exp $3 $5)]
     ;; mul(Expression, Expression)
     [(MUL LPAREN expression COMMA expression RPAREN)        (mul-exp $3 $5)]
     ;; quotient(Expression, Expression)
     [(QUOTIENT LPAREN expression COMMA expression RPAREN)   (quotient-exp $3 $5)]

     ;; Las extensiones de los booleanos
     ;; true
     [(TRUE)                                                 (true-exp)]
     ;; false
     [(FALSE)                                                (false-exp)]
     ;; equal?(Expression, Expression)
     [(EQUAL? LPAREN expression COMMA expression RPAREN)     (equal?-exp $3 $5)]
     ;; less?(Expression, Expression)
     [(LESS? LPAREN expression COMMA expression RPAREN)      (less?-exp $3 $5)]
     ;; greater?(Expression, Expression)
     [(GREATER? LPAREN expression COMMA expression RPAREN)   (greater?-exp $3 $5)]

     ;; Las operaciones con cualquier número de argumentos
     ;; +(Expressions)
     [(PLUS LPAREN expressions RPAREN)                       (plus-exp $3)]
     ;; *(Expressions)
     [(TIMES LPAREN expressions RPAREN)                      (times-exp $3)]

     ;; Las extensiones de las listas
     ;; emptylist
     [(EMPTYLIST)                                            (emptylist-exp)]
     ;; cons(Expression, Expression)
     [(CONS LPAREN expression COMMA expression RPAREN)       (cons-exp $3 $5)]
     ;; car(Expression)
     [(CAR LPAREN expression RPAREN)                         (car-exp $3)]
     ;; cdr(Expression)
     [(CDR LPAREN expression RPAREN)                         (cdr-exp $3)]
     ;; null?(Expression)
     [(NULL? LPAREN expression RPAREN)                       (null?-exp $3)]

     ;; Los constructos de la tarea
     ;; not(Expression)
     [(NOT LPAREN expression RPAREN)                         (not-exp $3)]
     ;; and(Expression, Expression)
     [(AND LPAREN expression COMMA expression RPAREN)        (and-exp $3 $5)]
     ;; (escribe la producción de or)
     [(OR LPAREN expression COMMA expression RPAREN)         (or-exp $3 $5)]
     ;; (escribe la producción de xor)
     [(XOR LPAREN expression COMMA expression RPAREN)        (xor-exp $3 $5)]
     ;; cond {Expression ==> Expression}* end
     [(COND branches END)                                    (cond-exp (map car $2)
                                                                       (map cdr $2))]
     ;; list(Expressions)
     [(LIST LPAREN expressions RPAREN)                       (list-exp $3)]
     ;; (escribe la producción de unpack)
     [(UNPACK identifiers EQ expression IN expression)       (unpack-exp $2 $4 $6)])

    ;; {Expression ==> Expression}*: cero o más ramas. Cada rama sale como un
    ;; par (prueba . resultado), y la regla de cond separa los pares en las
    ;; dos listas que cond-exp guarda.
    (branches
     [()                                       '()]
     [(expression ARROW expression branches)   (cons (cons $1 $3) $4)])

    ;; Identifier*: cero o más nombres, sin comas.
    (identifiers
     [()                                       '()]
     [(ID identifiers)                         (cons $1 $2)])

    ;; Expressions y NonemptyExpressions: cero o más expresiones, separadas
    ;; por comas. Dos no terminales para que +(1,) no pase: expressions admite
    ;; la lista vacía, que es la regla de ε, y nonempty-expressions exige una
    ;; expresión después de cada coma. Cada regla construye la lista de Racket
    ;; que el struct guarda.
    (expressions
     [()                                       '()]
     [(nonempty-expressions)                   $1])
    (nonempty-expressions
     [(expression)                             (list $1)]
     [(expression COMMA nonempty-expressions)  (cons $1 $3)]))))

;; La función generada se llama parse-tokens, y no parser, porque parser es
;; el nombre de la forma de parser-tools que la construye. Igual que pasó
;; con lexer y next-token.
;;
;; parse-tokens NO recibe una lista de tokens. Recibe un PROVEEDOR: una
;; función sin argumentos que, cada vez que se llama, devuelve el siguiente
;; token. El parser la llama cuando necesita un token más, hasta que recibe
;; EOF. Por eso next-token está hecho como está: un token por llamada.

;; ---------------------------------------------------------------------------
;; 4. DE LA CADENA AL ÁRBOL
;;
;; Junta las dos etapas. Abre un puerto sobre la cadena, define el proveedor
;; de tokens sobre ese puerto, y se lo entrega al parser.
;; ---------------------------------------------------------------------------

(define (parse str)
  (define port (open-input-string str))
  (define (next) (next-token port))
  (parse-tokens next))

;; ===========================================================================
;; PARA PROBAR EN EL REPL
;;
;;   > (parse "let x = 7 in -(x, 2)")
;;   (let-exp 'x (const-exp 7) (diff-exp (var-exp 'x) (const-exp 2)))
;;
;; Exactamente el árbol que tecleábamos a mano. Lo único que cambió es quién
;; lo construye.
;;
;;   > (parse "-(x-1, 2)")
;;   (diff-exp (var-exp 'x-1) (const-exp 2))
;;
;; x-1 es un solo nombre, como decidió el lexer. El parser recibe los tokens
;; hechos y no los vuelve a partir.
;;
;; Lo que el lexer dejó pasar, el parser lo detiene:
;;
;;   > (parse "zero?(3 3")
;;   parse: no esperaba NUM (3)
;;   > (parse "let x = in 3")
;;   parse: no esperaba IN
;;   > (parse "-(3, 4")
;;   parse: el texto se acabó a la mitad de una expresión
;;
;; Y un carácter que el lexer no reconoce sigue siendo error del lexer:
;;
;;   > (parse "3 @ 4")
;;   lexer: carácter inesperado: @
;;
;; Las extensiones. El parser construye el árbol de lo que el texto dice, sea
;; núcleo o sea azúcar: add sale como add-exp, y quien lo traduce es desugar.
;;
;;   > (parse "minus(-(3, 4))")
;;   (minus-exp (diff-exp (const-exp 3) (const-exp 4)))
;;   > (parse "add(1, 2)")
;;   (add-exp (const-exp 1) (const-exp 2))
;; ===========================================================================
