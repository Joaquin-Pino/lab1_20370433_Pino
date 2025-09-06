#lang racket

(require "utilidades.rkt")


; definicion
(define (crear-biblioteca  libros usuarios prestamos max-libros dias-max
        tasa-multa limite-deuda dias-retraso fecha-inicial) ;RF05
  (list libros usuarios prestamos max-libros dias-max
        tasa-multa limite-deuda dias-retraso fecha-inicial))

;selectores
(define (libros-en-biblioteca))
(define (libro-en-biblioteca? biblioteca libro) (member libro biblioteca))

;modificadores
(define (agregar-libro biblioteca libro) ;RF06
  (if ((not libro-en-biblioteca? biblioteca libro))
      ;agregamos el libro
      (modificar-elemento biblioteca 0 (append )
      #f))