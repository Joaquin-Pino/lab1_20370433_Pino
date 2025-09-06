#lang racket

(require "utilidades.rkt")

; definicion de prestamo
(define (crear-prestamo id id-usuario id-libro fecha-prestamo dias-solicitados)
  (list id id-usuario id-libro fecha-prestamo dias-solicitados))

;selectores
(define (id-prestamo prestamo) (list-ref prestamo 0))
(define (id-usuario-prestamo prestamo) (list-ref prestamo 1))
(define (id-libro-prestado prestamo) (list-ref prestamo 2))
(define (fecha-prestamo prestamo) (list-ref prestamo 3))
(define (duracion-prestamo prestamo) (list-ref prestamo 4))

(define prest (crear-prestamo 123 01 90 "01/09" 5))

  