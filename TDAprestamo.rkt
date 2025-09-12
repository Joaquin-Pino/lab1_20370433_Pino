#lang racket

(require "utilidades.rkt")
(require "TDAdia.rkt")

;-----------------------------definicion-----------------------------
; crea tipo de dato prestamo
; dominio: int, int, int, str, int
; recorrido: prestamo
(define (crear-prestamo id id-usuario id-libro fecha-prestamo dias-solicitados)
  (list id id-usuario id-libro fecha-prestamo dias-solicitados))

;-----------------------------selectores-----------------------------
; obtiene id de prestamo
; domino: pestamo
; recorrido: int
(define (id-prestamo prestamo) (obtener-dato prestamo 0))

; obtiene id de usuario que realizo el prestamo
; domino: pestamo
; recorrido: int
(define (id-usuario-prestamo prestamo) (obtener-dato prestamo 1))

; obtiene id del libro prestado
; domino: pestamo
; recorrido: int
(define (id-libro-prestado prestamo) (obtener-dato prestamo 2))

; obtiene fecha del prestamo
; domino: pestamo
; recorrido: fecha
(define (fecha-prestamo prestamo) (obtener-dato prestamo 3))

; obtiene duracion del prestamo
; domino: pestamo
; recorrido: int
(define (duracion-prestamo prestamo) (obtener-dato prestamo 4))



;pertenencia
; verifica si tipo de dato es de tipo de dato prestamo
; dominio: cualquier tipo de dato
; recorrido: bool
(define (prestamo? p)
  (and (number? (id-prestamo p))
       (number? (id-usuario-prestamo p))
       (number? (id-libro-prestado p))
       (string? (fecha-prestamo p))
       (number? (duracion-prestamo p))
       (= (length p) 5)))


;;----------------------------- otro ------------------------------
(define (obtener-fecha-vencimiento prestamo)
  (let ((fecha-prest (leer-fecha (fecha-prestamo prestamo)))
        (dias-prestado (duracion-prestamo prestamo)))
    (fecha->string(sumar-dias fecha-prest dias-prestado))
    ))


;------------------------------ pruebas
(define p1 (crear-prestamo 01 01 01 "28/01" 5)) 
(obtener-fecha-vencimiento p1)

