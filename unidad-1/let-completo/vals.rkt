#lang racket

(provide
 (contract-out
  
  ;; Constructores de valores LET
  ;; Pasar de mundo Racket a mundo LET
  ;; Salen como struct, y no solo como función constructora, para que un
  ;; match de otro módulo pueda usarlos de patrón: [(intval n) ...]
  [struct intval ([n exact-integer?])]
  [struct boolval ([b boolean?])]
  [struct listval ([vs (listof expval?)])]
  
  ;; Predicados de valores expresados y denotados
  [expval? (-> any/c boolean?)]
  [denval? (-> any/c boolean?)]
  
  ;; Pasar de mundo LET a mundo Racket
  [val->int (-> any/c exact-integer?)]
  [val->bool (-> any/c boolean?)]
  [val->list (-> any/c (listof expval?))]))

(struct intval (n) #:transparent)
(struct boolval (b) #:transparent)

;; Una lista de LET guarda una lista de Racket cuyos elementos son valores
;; de LET: (listval (list (intval 1) (boolval #t))). La lista vacía de LET
;; es (listval '()).
(struct listval (vs) #:transparent)

(define (expval? obj)
  (or (intval? obj)
      (boolval? obj)
      (listval? obj)))

(define (denval? obj)
  (expval? obj))

(define (val->int obj)
  (match obj
    [(intval n) n]
    [_ (error 'val->int
              "Esperaba un entero pero me diste ~a" obj)]))

(define (val->bool obj)
  (match obj
    [(boolval b) b]
    [_ (error 'val->bool
              "Esperaba un booleano pero me diste ~a" obj)]))

(define (val->list obj)
  (match obj
    [(listval vs) vs]
    [_ (error 'val->list
              "Esperaba una lista pero me diste ~a" obj)]))
