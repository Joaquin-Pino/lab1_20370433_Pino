#lang racket

(provide modificar-elemento)
(define (modificar-elemento tipo-dato indx-dato nuevo)
  ; TODO: manejar caso de lista vacia
  ; manejar caso que se pase un indice mas grande que cantidad de elementos en la lista
  (define (aux n lista)
    (if (= n indx-dato)
        (cons nuevo (cdr lista))
        (cons (car lista) (aux (+ n 1) (cdr lista)))))
  (aux 0 tipo-dato))
