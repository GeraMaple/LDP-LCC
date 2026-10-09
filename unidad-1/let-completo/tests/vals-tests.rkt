#lang racket

;; ===========================================================================
;; Tests unitarios de vals.rkt: los valores expresados y denotados de LET
;;
;; Se prueba el módulo por su interfaz, como cualquier cliente: este archivo
;; hace require de vals.rkt y por eso sus llamadas cruzan la frontera y pasan
;; por los contratos. Un submódulo test adentro de vals.rkt no lo haría.
;;
;; CÓMO CORRERLO
;;   raco test tests/vals-tests.rkt      desde let-completo/
;;   raco test tests                     corre todos los archivos de tests/
;;
;; Los casos siguen los tres papeles de la interfaz: constructores,
;; predicados y extractores.
;; ===========================================================================

(require rackunit
         "../vals.rkt")

;; ---------------------------------------------------------------------------
;; 0. Por qué las demás pruebas pueden comparar valores
;;
;; check-equal? compara con equal?, y equal? ve adentro de la cajita porque
;; la struct es #:transparent. Sin eso, dos (intval 5) construidos aparte
;; serían distintos y ninguna prueba de value-of podría escribirse así.
;; ---------------------------------------------------------------------------

(check-equal? (intval 5) (intval 5)
              "dos cajitas con el mismo entero son iguales")

;; ---------------------------------------------------------------------------
;; Constructores: el contrato de entrada rechaza lo que no es del tipo
;; ---------------------------------------------------------------------------

(check-exn exn:fail:contract?
           (lambda () (intval #t))
           "un booleano no entra en intval")

(check-exn exn:fail:contract?
           (lambda () (intval 6.0))
           "6.0 no es un entero exacto")

(check-exn exn:fail:contract?
           (lambda () (boolval 0))
           "el cero no es falso, no hay conversión")

;; ---------------------------------------------------------------------------
;; Predicados: responden, no fallan
;; ---------------------------------------------------------------------------

(check-true (expval? (intval 5))
            "un entero de LET es un valor expresado")

(check-true (expval? (boolval #f))
            "un booleano de LET es un valor expresado")

(check-false (expval? 5)
             "un entero de Racket no es un valor de LET")

(check-true (denval? (boolval #t))
            "lo expresable es denotable")

(check-false (denval? "hola")
             "LET no tiene cadenas")

;; ---------------------------------------------------------------------------
;; Extractores: ida y vuelta, y el caso cruzado
;;
;; check-exn con un predicado pide un tipo de error. Con una expresión regular
;; pide un mensaje. El error del extractor es un exn:fail común, no una
;; violación de contrato, y la expresión regular es lo que lo distingue.
;; ---------------------------------------------------------------------------

(check-equal? (val->int (intval 5)) 5
              "del entero de LET sale el entero de Racket")

(check-equal? (val->bool (boolval #f)) #f
              "#f es un valor válido, no una falla")

(check-exn #rx"Esperaba un entero"
           (lambda () (val->int (boolval #t)))
           "el error del caso cruzado es del extractor")

(check-exn #rx"Esperaba un booleano"
           (lambda () (val->bool (intval 0)))
           "simétrico")

(check-exn #rx"Esperaba un entero"
           (lambda () (val->int 5))
           "any/c deja pasar un valor de Racket y el extractor lo rechaza")

;; ---------------------------------------------------------------------------
;; Las listas: el tercer valor expresado
;; ---------------------------------------------------------------------------

(check-equal? (listval (list (intval 1) (intval 2)))
              (listval (list (intval 1) (intval 2)))
              "dos listas con los mismos valores son iguales")

(check-exn exn:fail:contract?
           (lambda () (listval (list 1 2)))
           "adentro de una lista de LET van valores de LET, no enteros de Racket")

(check-exn exn:fail:contract?
           (lambda () (listval 5))
           "una lista de LET guarda una lista de Racket")

(check-true (expval? (listval '()))
            "la lista vacía es un valor expresado")

(check-true (denval? (listval (list (boolval #t))))
            "una lista es denotable: puede vivir en el entorno")

(check-equal? (val->list (listval (list (intval 1)))) (list (intval 1))
              "de la lista de LET sale la lista de Racket, con sus cajitas")

(check-equal? (val->list (listval '())) '()
              "la lista vacía sale como la lista vacía de Racket")

(check-exn #rx"Esperaba una lista"
           (lambda () (val->list (intval 5)))
           "el error del caso cruzado es del extractor")
