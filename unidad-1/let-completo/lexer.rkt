#lang racket

;; ===========================================================================
;; Lexer del lenguaje LET
;;
;; Hasta ahora los programas de LET los hemos escrito como árboles, a mano:
;;
;;   (let-exp 'x (const-exp 7) (diff-exp (var-exp 'x) (const-exp 2)))
;;
;; Nadie programa así. Un programa se escribe como texto:
;;
;;   "let x = 7 in -(x, 2)"
;;
;; Del texto al árbol hay dos etapas, y este archivo es la primera:
;;
;;   texto ---[lexer]---> tokens ---[parser]---> árbol ---[value-of]---> valor
;;
;; El lexer parte el texto en piezas. Cada pieza se llama LEXEMA, y la
;; clasificación de esa pieza (es un número, es un nombre, es la palabra
;; reservada let, es un paréntesis) se llama TOKEN. De "let x = 7 in -(x, 2)"
;; salen once tokens: LET, ID x, EQ, NUM 7, IN, MINUS, LPAREN, ID x, COMMA,
;; NUM 2, RPAREN, y al final uno más, EOF, que dice que el texto se acabó.
;;
;; Lo que el lexer NO hace es revisar que los tokens estén en un orden que
;; tenga sentido. "zero?(3 3" pasa el lexer sin queja: son cuatro tokens
;; válidos. Que falte una coma y un paréntesis lo nota el parser, que es quien
;; conoce la gramática.
;;
;; El lexer no se escribe a mano. Se DESCRIBE con expresiones regulares y la
;; biblioteca parser-tools genera el código que las reconoce.
;; ===========================================================================

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; parser-tools/lex trae lexer, define-tokens y compañía. parser-tools/lex-sre
;; trae los operadores de las expresiones regulares (:: :or :* :+ :? :/), y el
;; prefix-in les pone dos puntos al frente para que no choquen con nada de
;; Racket: :+ es el operador de lex-sre, + es la suma de siempre.

(provide
 (contract-out
  ;; El lexer en sí: lee del puerto y devuelve el siguiente token
  [next-token (-> input-port?
                  let-token?)]

  ;; Herramienta para esta sesión: todos los tokens de una cadena, en lista
  [tokenize (-> string? (listof let-token?))]

  ;; Predicado de tokens de LET
  [let-token? (-> any/c boolean?)])

 ;; Los dos grupos de tokens. El parser los va a necesitar para saber con
 ;; qué tokens trabaja.
 value-tokens
 empty-tokens

 ;; Selectores de parser-tools, re-exportados para que quien use este archivo
 ;; no tenga que requerir parser-tools también.
 token-name
 token-value)

;; ---------------------------------------------------------------------------
;; 1. LOS TOKENS
;;
;; Hay dos clases de token:
;;
;;   - Los que CARGAN UN VALOR. Un número no es solo "un número": es el 7 o el
;;     -420. Un nombre no es solo "un nombre": es x o es w. El token guarda el
;;     lexema convertido: el entero, o el símbolo.
;;
;;   - Los que NO cargan valor. Un paréntesis izquierdo es un paréntesis
;;     izquierdo y ya. La palabra reservada let es let y ya.
;;
;; define-tokens declara los primeros y define-empty-tokens los segundos.
;; Cada grupo recibe un nombre, value-tokens y empty-tokens, y por cada token
;; T se define un constructor token-T.
;; ---------------------------------------------------------------------------

(define-tokens       value-tokens (NUM ID))
(define-empty-tokens empty-tokens (MINUS ZERO? IF THEN ELSE LET EQ IN
                                   LPAREN RPAREN COMMA EOF
                                   ;; Las extensiones de la aritmética
                                   MINUS-KW ADD MUL QUOTIENT
                                   ;; Las extensiones de los booleanos
                                   TRUE FALSE EQUAL? LESS? GREATER?
                                   ;; Las operaciones con cualquier número de argumentos
                                   PLUS TIMES
                                   ;; Las extensiones de las listas
                                   EMPTYLIST CONS CAR CDR NULL?
                                   ;; Los constructos de la tarea
                                   NOT AND OR XOR COND ARROW END LIST UNPACK))

;; Los doce tokens del lenguaje LET, uno por cada clase de terminal de la
;; gramática, más EOF, que no está en el texto: lo produce el lexer cuando el
;; texto se acaba. El parser lo necesita para saber que ya no hay más.
;;
;; Después vienen los cuatro de la aritmética: una palabra reservada por
;; constructo. La palabra minus necesita un nombre de token distinto del
;; signo -, que ya se llama MINUS, y por eso se llama MINUS-KW (de keyword,
;; palabra reservada). Y después los cinco de los booleanos.
;;
;; Un token con valor es un struct de parser-tools con dos campos:
;;
;;   > (token-NUM 7)
;;   (token 'NUM 7)
;;   > (token-name (token-NUM 7))
;;   'NUM
;;   > (token-value (token-NUM 7))
;;   7
;;
;; Un token sin valor parser-tools lo representa con el puro símbolo:
;;
;;   > (token-LET)
;;   'LET
;;   > (token-name (token-LET))
;;   'LET
;;   > (token-value (token-LET))
;;   #f
;;
;; token-name y token-value funcionan con los dos. Eso es lo que importa.

;; ---------------------------------------------------------------------------
;; 2. LAS CATEGORÍAS LÉXICAS, DESCRITAS CON EXPRESIONES REGULARES
;;
;; La especificación de LET define Number e Identifier con producciones:
;;
;;   Digit       ::= 0 | 1 | ... | 9
;;   Number      ::= Digit+
;;                |  - Digit+
;;   Alpha       ::= a | ... | z | A | ... | Z
;;   Alphanumesp ::= Alpha | Digit | _ | - | ?
;;   Identifier  ::= Alpha Alphanumesp*
;;
;; Cada una es una expresión regular, la misma clase de descripción que se
;; escribe en UNIX para grep o sed. lex-sre las escribe con paréntesis y un
;; operador al frente en vez de con símbolos pegados. La tabla:
;;
;;   UNIX              lex-sre            se lee
;;   ----------------  -----------------  ---------------------------------
;;   a|b               (:or a b)          a, o bien b
;;   ab                (:: a b)           a seguido de b
;;   a*                (:* a)             cero o más a
;;   a+                (:+ a)             una o más a
;;   a?                (:? a)             cero o una a
;;   [0-9]             (:/ #\0 #\9)       cualquier carácter entre 0 y 9
;;   let               "let"              exactamente ese texto
;;
;; Y las dos categorías completas, en las dos notaciones:
;;
;;   -?[0-9]+                      (:: (:? "-") (:+ digit))
;;   [a-zA-Z][a-zA-Z0-9_?-]*       (:: alpha (:* alnum-esp))
;;
;; define-lex-abbrevs les pone nombre, igual que la gramática les pone nombre
;; a sus producciones. Una abreviatura puede usar a las anteriores.
;; ---------------------------------------------------------------------------

(define-lex-abbrevs
  [digit      (:/ #\0 #\9)]
  [alpha      (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [alnum-esp  (:or alpha digit "_" "-" "?")]
  [number     (:: (:? "-") (:+ digit))]
  [identifier (:: alpha (:* alnum-esp))])

;; number junta las dos producciones de Number en una: el guion es opcional
;; y después vienen uno o más dígitos. identifier es la traducción directa:
;; una letra y después cero o más caracteres de alnum-esp.
;;
;; Fíjate en que alnum-esp incluye el guion y el signo de interrogación. Es
;; lo que la especificación dice, y tiene consecuencias: x-1 es UN
;; identificador, no x menos 1, y zero? tiene la forma de un identificador.
;; Las dos cosas se vuelven a ver abajo.

;; ---------------------------------------------------------------------------
;; 3. EL LEXER
;;
;; lexer recibe una lista de reglas [expresión-regular acción]. Cuando se
;; llama con un puerto de entrada, lee del puerto el lexema más largo que
;; alguna regla reconozca, y ejecuta la acción de esa regla. Dentro de la
;; acción, lexeme es el texto que acaba de reconocer e input-port es el
;; puerto del que está leyendo.
;;
;; Casi siempre hay más de una regla que podría aplicar. Dos reglas de
;; desempate, y las dos importan:
;;
;;   [1] GANA EL LEXEMA MÁS LARGO. Frente a "-5", la regla de number reconoce
;;       dos caracteres y la de "-" reconoce uno: gana number y sale NUM -5.
;;       Frente a "-(", number no reconoce nada (después del guion no hay
;;       dígito), así que gana "-" y sale MINUS. Frente a "zero?z", la palabra
;;       reservada reconoce cinco caracteres e identifier reconoce seis: gana
;;       identifier y sale ID zero?z.
;;
;;   [2] A IGUAL LONGITUD, GANA LA REGLA QUE ESTÁ MÁS ARRIBA. Frente a
;;       "zero?", la palabra reservada e identifier reconocen los mismos
;;       cinco caracteres. Gana la que se escribió primero. Por eso las seis
;;       palabras reservadas van ANTES que identifier: si identifier fuera
;;       primero, let, if y las demás saldrían como ID y el lenguaje no
;;       tendría palabras reservadas.
;;
;; La función se llama next-token, y no lexer, porque lexer es el nombre de la
;; forma de parser-tools que la construye: cada llamada devuelve el siguiente
;; token del puerto.
;; ---------------------------------------------------------------------------

(define next-token
  (lexer
   ;; Números. Con el guion opcional adentro, -5 es un solo token.
   [number      (token-NUM (string->number lexeme))]

   ;; Palabras reservadas. Van antes que identifier por la regla [2].
   ["zero?"     (token-ZERO?)]
   ["if"        (token-IF)]
   ["then"      (token-THEN)]
   ["else"      (token-ELSE)]
   ["let"       (token-LET)]
   ["in"        (token-IN)]

   ;; Las palabras reservadas de la aritmética. También antes que
   ;; identifier, por la misma regla [2].
   ["minus"     (token-MINUS-KW)]
   ["add"       (token-ADD)]
   ["mul"       (token-MUL)]
   ["quotient"  (token-QUOTIENT)]

   ;; Las palabras reservadas de los booleanos.
   ["true"      (token-TRUE)]
   ["false"     (token-FALSE)]
   ["equal?"    (token-EQUAL?)]
   ["less?"     (token-LESS?)]
   ["greater?"  (token-GREATER?)]

   ;; Las palabras reservadas de las listas.
   ["emptylist" (token-EMPTYLIST)]
   ["cons"      (token-CONS)]
   ["car"       (token-CAR)]
   ["cdr"       (token-CDR)]
   ["null?"     (token-NULL?)]

   ;; Las palabras reservadas de la tarea.
   ["not"       (token-NOT)]
   ["and"       (token-AND)]
   ["or"        (token-OR)]
   ["xor"       (token-XOR)]
   ["cond"      (token-COND)]
   ["end"       (token-END)]
   ["list"      (token-LIST)]
   ["unpack"    (token-UNPACK)]

   ;; Nombres. Lo que tiene forma de identificador y no es palabra reservada.
   [identifier  (token-ID (string->symbol lexeme))]

   ;; Signos de un solo carácter.
   ["-"         (token-MINUS)]
   ["="         (token-EQ)]
   ;; La flecha de cond mide tres caracteres. Frente a "==>" le gana a "="
   ;; por la regla [1], el lexema más largo.
   ["==>"       (token-ARROW)]
   ["+"         (token-PLUS)]
   ["*"         (token-TIMES)]
   ["("         (token-LPAREN)]
   [")"         (token-RPAREN)]
   [","         (token-COMMA)]

   ;; Espacios, tabuladores y saltos de línea. No producen token: el lexer se
   ;; llama a sí mismo sobre el mismo puerto para leer el siguiente.
   [whitespace  (next-token input-port)]

   ;; Se acabó el texto.
   [(eof)       (token-EOF)]

   ;; Cualquier otro carácter. Sin esta regla parser-tools lanza su propio
   ;; error, que es más difícil de leer. any-char reconoce exactamente un
   ;; carácter, así que solo gana cuando ninguna otra regla reconoce nada.
   [any-char    (error 'lexer "carácter inesperado: ~a" lexeme)]))

;; ---------------------------------------------------------------------------
;; 4. ¿QUÉ ES UN TOKEN DE LET?
;;
;; parser-tools trae un predicado token?, pero da #f para los tokens sin
;; valor, que son símbolos. Y da #t para cualquier token con valor, aunque sea
;; de otro lenguaje. Ninguna de las dos cosas sirve para un contrato.
;;
;; Un token de LET es algo cuyo nombre está en la lista: los doce del núcleo
;; más los cuatro de la aritmética, los cinco de los booleanos, los signos
;; + y *, los cinco de las listas y los nueve de la tarea.
;; ---------------------------------------------------------------------------

(define token-names
  '(NUM ID MINUS ZERO? IF THEN ELSE LET EQ IN LPAREN RPAREN COMMA EOF
    MINUS-KW ADD MUL QUOTIENT
    TRUE FALSE EQUAL? LESS? GREATER?
    PLUS TIMES
    EMPTYLIST CONS CAR CDR NULL?
    NOT AND OR XOR COND ARROW END LIST UNPACK))

(define (let-token? obj)
  (and (or (token? obj) (symbol? obj))
       (memq (token-name obj) token-names)
       #t))

;; ---------------------------------------------------------------------------
;; 5. TODOS LOS TOKENS DE UNA CADENA
;;
;; next-token devuelve UN token por llamada. Es así a propósito: el parser le
;; va a pedir los tokens de uno en uno, conforme los necesite. Pero para
;; estudiar el lexer conviene ver la secuencia completa.
;;
;; tokenize abre un puerto sobre la cadena y llama a next-token hasta que sale
;; EOF. EOF va incluido en la lista: es un token como los demás.
;; ---------------------------------------------------------------------------

(define (tokenize str)
  (define port (open-input-string str))
  (define (loop)
    (define tok (next-token port))
    (if (eq? (token-name tok) 'EOF)
        (list tok)
        (cons tok (loop))))
  (loop))

;; ===========================================================================
;; PARA PROBAR EN EL REPL
;;
;;   > (tokenize "let x = 7 in -(x, 2)")
;;   (list 'LET (token 'ID 'x) 'EQ (token 'NUM 7) 'IN 'MINUS 'LPAREN
;;         (token 'ID 'x) 'COMMA (token 'NUM 2) 'RPAREN 'EOF)
;;
;; Los espacios desaparecieron. Los tokens sin valor salen como símbolos y
;; los que cargan valor salen como (token nombre valor).
;;
;;   > (map token-name (tokenize "let x = 7 in -(x, 2)"))
;;   '(LET ID EQ NUM IN MINUS LPAREN ID COMMA NUM RPAREN EOF)
;;
;; Las dos reglas de desempate, vistas:
;;
;;   > (tokenize "-(x-1, -5)")
;;   (list 'MINUS 'LPAREN (token 'ID 'x-1) 'COMMA (token 'NUM -5) 'RPAREN 'EOF)
;;   > (tokenize "zero?(y)")
;;   (list 'ZERO? 'LPAREN (token 'ID 'y) 'RPAREN 'EOF)
;;   > (tokenize "zero?y")
;;   (list (token 'ID 'zero?y) 'EOF)
;;
;; Y el error propio:
;;
;;   > (tokenize "3 @ 4")
;;   lexer: carácter inesperado: @
;;
;; Las palabras de la aritmética, y el signo menos junto a la palabra minus:
;;
;;   > (tokenize "minus(-(3, 4))")
;;   (list 'MINUS-KW 'LPAREN 'MINUS 'LPAREN (token 'NUM 3) 'COMMA
;;         (token 'NUM 4) 'RPAREN 'RPAREN 'EOF)
;; ===========================================================================
