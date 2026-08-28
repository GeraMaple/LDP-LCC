#lang racket

(struct num-exp (n) #:transparent)

(struct id-exp (n) #:transparent)

(struct add-exp (izq der) #:transparent)

(struct mul-exp (izq der) #:transparent)

(struct with-exp (nombre expresion cuerpo) #:transparent)

(define (asocs-vacias) '())

(define (extender asocs nombre valor)
  (cons (cons nombre valor) asocs))

(define (buscar nombre asocs)
  (cdr (assoc nombre asocs)))

(define (calc e asocs)
  (match e
    [(num-exp n) n]

    [(id-exp n)
     (buscar n asocs)]

    [(add-exp n1 n2)
     (+ (calc n1 asocs) (calc n2 asocs))]

    [(mul-exp n1 n2)
     (* (calc n1 asocs) (calc n2 asocs))]

    [(with-exp nombre expresion cuerpo)
     (calc cuerpo 
           (extender asocs
                     nombre
                     (calc expresion asocs)))]))


#/
with x = add(2,3) in with y =mul(x,2) in add(x, y)

Abreviaturas:
N2 = (num-exp 2)
N3 = (num-exp 3)
Ix = (id-exp 'x)
Iy = (id-exp 'y)
A = (add-exp N2 N3)
M = (mul-exp Ix N2)
B = (add-exp Ix Iy)

  
  (calc (with-exp 'x A (with-exp 'y M B)) σ0)
= (calc (with-exp 'y M B) extend(σ0, 'x, (calc A σ0)))                    [with]
= (calc (with-exp 'y M B) extend(σ0, 'x, (calc N2 σ0) + (calc N3 σ0)))    [add]
= (calc (with-exp 'y M B) extend(σ0, 'x, 2 + (calc N3 σ0)))               [num]
= (calc (with-exp 'y M B) extend(σ0, 'x, 2 + 3))                          [num]
= (calc (with-exp 'y M B) extend(σ0, 'x, 5))

  con σ1 = extend(σ0, 'x, 5)

= (calc B extend(σ1,'y,(calc M σ1)))                      [with]
= (calc B extend(σ1, 'y, (calc Ix σ1) * (calc N2 σ1)))      [mul]
= (calc B extend(σ1,'y,5*(calc N2 σ1)))                 [id]
= (calc B  extend(σ1,'y, 5 * 2))                           [num]
= (calc B extend(σ1, 'y, 10))

  con σ2 = extend(σ1, 'y, 10)

= (calc Ix σ2) + (calc Iy σ2)                           [add]
= 5 + (calc Iy σ2)                                     [id]
= 5 + 10                                               [id]
= 15
#/
