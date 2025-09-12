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

; obtiene dato de indice indicado
; domino: lista
; recorrido; tipo-dato
(provide obtener-dato)
(define (obtener-dato lista pos)
  (list-ref lista pos))

; agrega cosas a una lista, wrapper de cons
; dominio: tipo-dato, lista
; recorrido: lista
(provide agregar-inicio-lista) 
(define (agregar-inicio-lista cosa lista)
  (cons cosa lista))

(provide agregar-final-lista)
(define (agregar-final-lista elemento lista)
  (cond
    ((null? lista) (cons elemento lista))
    (else (cons (car lista) (agregar-final-lista elemento (cdr lista)))))
    )
