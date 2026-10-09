#lang racket

;; Entornos del lenguaje LET
(require "vals.rkt")

;;(require "env-impl-lists.rkt")
(require "env-impl-struct.rkt")

(provide
 (contract-out
  ;; Interfaz de entornos
  [env? (-> any/c boolean?)]
  [empty-env (-> env?)]
  [apply-env (-> env? symbol? denval?)]
  [extend-env (-> symbol? denval? env? env?)]

  ;; Operaciones auxiliares "env-utils.rkt"
  [list->env (-> (listof (cons/c symbol? denval?))
                 env?)]))

(define (list->env lis)
  (if (null? lis)
      (empty-env)
      (extend-env (car (first lis))
                  (cdr (first lis))
                  (list->env (rest lis)))))
