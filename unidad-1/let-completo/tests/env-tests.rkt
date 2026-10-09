#lang racket

;; ===========================================================================
;; Tests unitarios de env.rkt: los entornos de LET
;;
;; Se prueba la interfaz, no una representación. Este archivo hace require de
;; env.rkt, que a su vez elige una implementación con su línea de require.
;; Para correr los mismos tests contra la otra implementación, cambia esa
;; línea en env.rkt y vuelve a correr. Nada de aquí tiene que cambiar.
;;
;; Los entornos son opacos, así que ninguna prueba compara dos entornos con
;; check-equal?. Todo se observa por apply-env, que es la única ventana que
;; la interfaz da. Una prueba como (check-true (env? '())) pasaría con la
;; representación de listas y fallaría con la de estructuras: dependería de
;; la representación, y por eso no está aquí.
;;
;; CÓMO CORRERLO
;;   raco test tests/env-tests.rkt       desde let-completo/
;; ===========================================================================

(require rackunit
         "../vals.rkt"
         "../env.rkt")

;; ---------------------------------------------------------------------------
;; Las tres operaciones: las ecuaciones del entorno hechas prueba
;; ---------------------------------------------------------------------------

(check-true (env? (empty-env))
            "el entorno vacío es un entorno")

(check-exn #rx"la variable x no está vinculada"
           (lambda () (apply-env (empty-env) 'x))
           "buscar en el entorno vacío es error, y el mensaje dice cuál variable")

(check-equal? (apply-env (extend-env 'x (intval 7) (empty-env)) 'x)
              (intval 7)
              "[x=v]ρ(x) = v")

(check-equal? (apply-env (extend-env 'y (intval 1)
                           (extend-env 'x (intval 7) (empty-env)))
                         'x)
              (intval 7)
              "extender no borra lo anterior: [y=w]ρ(x) = ρ(x)")

(check-equal? (apply-env (extend-env 'x (intval 42)
                           (extend-env 'x (intval 666) (empty-env)))
                         'x)
              (intval 42)
              "eclipsado: gana la vinculación de adentro")

(check-equal? (apply-env (extend-env 'x (boolval #t) (empty-env)) 'x)
              (boolval #t)
              "el entorno guarda booleanos, no solo enteros")

;; ---------------------------------------------------------------------------
;; Los contratos de la interfaz
;; ---------------------------------------------------------------------------

(check-exn exn:fail:contract?
           (lambda () (extend-env 'x 7 (empty-env)))
           "un 7 sin cajita no entra al entorno")

(check-exn exn:fail:contract?
           (lambda () (apply-env (empty-env) "x"))
           "el nombre tiene que ser un símbolo")

;; ---------------------------------------------------------------------------
;; list->env: vive en la interfaz, así que se prueba aquí
;; ---------------------------------------------------------------------------

(check-equal? (apply-env (list->env (list (cons 'x (intval 1)))) 'x)
              (intval 1)
              "una lista de un par")

(check-exn #rx"no está vinculada"
           (lambda () (apply-env (list->env '()) 'x))
           "la lista vacía da el entorno vacío")

(check-equal? (apply-env (list->env (list (cons 'x (intval 42))
                                          (cons 'x (intval 666))))
                         'x)
              (intval 42)
              "el primer par de la lista queda al frente")

(check-exn exn:fail:contract?
           (lambda () (list->env (list (cons 'x 42))))
           "falla temprano: al construir, no al consultar")
