#lang racket

(require "utilidades.rkt")

;definicion
; crea la representacion de un libro
; dominio: int, int
; recorrido: fecha
(define (crear-fecha dia mes) (list dia mes))

;selectores
; obtiene el dia de una fecha
; dominio: fecha
; recorrido: int
(define (get-dia fecha) (obtener-dato fecha 0))

; obtiene el mes de una fecha
; dominio: fecha
; recorrido: int
(define (get-mes fecha) (obtener-dato fecha 1))

;modificadores
; modififica el dia de una fecha
; dominio: fecha
; recorrido: fecha
(define (modificar-dia fecha nuevo)
  (modificar-elemento fecha 0 nuevo))

; modififica el mes de una fecha
; dominio: fecha
; recorrido: fecha
(define (modificar-mes fecha nuevo)
  (modificar-elemento fecha 1 nuevo))

;pertenencia
; valida que el dato ingresado sea una fecha
; dominio: cualquier tipo de dato
; recorrido: bool
(define (fecha? fecha)
  (and (number? (get-dia fecha))
       (number? (get-mes fecha))
       (= (length fecha) 2)))

;otros

; lee fecha en formato dd/mm y entrega fecha
; dominio: string
; recorrido: fecha
(provide leer-fecha)
(define (leer-fecha str-fecha) 
  (let ((dia (substring str-fecha 0 2))
        (mes (substring str-fecha 3 4)))
    (crear-fecha (string->number dia) (string->number mes))))


;falta convertir fecha a string formato dd/mm
;falta funcion para calcular diferencia entre dos fechas
;funcion para sumar dias