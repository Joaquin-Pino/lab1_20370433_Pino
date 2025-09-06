#lang racket

(require "utilidades.rkt")

; definicion
(define (crear-libro id titulo autor) (list id titulo autor))

;selectores
(define (get-libro-id libro) (obtener-dato libro 0))
