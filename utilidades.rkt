#lang racket


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
