#lang racket


(provide modificar-elemento)
; modifica un elemento del tipo de dato
; domino: tipo-de-dato, int, tipo-de-dato
; recorrido: tip-de-dato
(define (modificar-elemento tipo-dato indx-dato nuevo)
  ; TODO: manejar caso de lista vacia
  ; manejar caso que se pase un indice mas grande que cantidad de elementos en la lista
  (define (aux n lista)
    (if (= n indx-dato)
        (cons nuevo (cdr lista))
        (cons (car lista) (aux (+ n 1) (cdr lista)))))
  (aux 0 tipo-dato))

(provide obtener-dato)
; obtiene dato de indice indicado
; domino: lista
; recorrido; tipo-dato
(define (obtener-dato lista pos)
  (list-ref lista pos))

(provide agregar)
; agrega cosas a una lista, wrapper de cons
; dominio: tipo-dato, lista
; recorrido: lista
(define (agregar cosa lista)
  (cons cosa lista))

(define (buscar-elemento predicado lista elemento)
  (cond
    ((null? lista) #f)
    ((predicado (car lista)) elemento)
    (else (buscar-primero predicado (cdr lista)))
)

      
