#lang racket

(require "utilidades.rkt")
(require "TDAdia.rkt")

;-----------------------------definicion-----------------------------
; crea tipo de dato prestamo
; dominio: int, int, int, str, int
; recorrido: prestamo
(provide crear-prestamo)
(define (crear-prestamo id id-usuario id-libro fecha-prestamo dias-solicitados)
  (list id id-usuario id-libro fecha-prestamo dias-solicitados #t)); bool->activo?

;-----------------------------selectores-----------------------------
; obtiene id de prestamo
; domino: pestamo
; recorrido: int
(provide id-prestamo)
(define (id-prestamo prestamo) (obtener-dato prestamo 0))

; obtiene id de usuario que realizo el prestamo
; domino: pestamo
; recorrido: int
(provide id-usuario-prestamo)
(define (id-usuario-prestamo prestamo) (obtener-dato prestamo 1))

; obtiene id del libro prestado
; domino: pestamo
; recorrido: int
(provide id-libro-prestado)
(define (id-libro-prestado prestamo) (obtener-dato prestamo 2))

; obtiene fecha del prestamo
; domino: pestamo
; recorrido: fecha
(provide fecha-prestamo)
(define (fecha-prestamo prestamo) (obtener-dato prestamo 3))

; obtiene duracion del prestamo
; domino: pestamo
; recorrido: int
(provide duracion-prestamo)
(define (duracion-prestamo prestamo) (obtener-dato prestamo 4))

; obtiene estado del prestamo
; domino: pestamo
; recorrido: bool
(provide get-estado-prestamo)
(define (get-estado-prestamo prestamo) (obtener-dato prestamo 5))

;-----------------------------modifcador-----------------------------
(provide cambiar-duracion-prestamo)
(define (cambiar-duracion-prestamo prestamo nueva-duracion)
  (crear-prestamo (id-prestamo prestamo) (id-usuario-prestamo prestamo) ((id-libro-prestado prestamo))
                  (fecha-prestamo prestamo) nueva-duracion))

(provide modificar-estado-prestamo)
(define (modificar-estado-prestamo prestamo nuevo-estado)
  (list (id-prestamo prestamo) (id-usuario-prestamo prestamo) (id-libro-prestado prestamo)
        (fecha-prestamo prestamo) (duracion-prestamo prestamo) nuevo-estado))
;-----------------------------pertenencia-----------------------------
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
;calcula la fecha de vencimiento
;dominio: prestamo
; recorrido:str
(provide obtener-fecha-vencimiento)
(define (obtener-fecha-vencimiento prestamo)
  (let ((fecha-prest (leer-fecha (fecha-prestamo prestamo)))
        (dias-prestado (duracion-prestamo prestamo)))
    (fecha->string(sumar-dias fecha-prest dias-prestado))
    ))


;------------------------------ pruebas
;(define p1 (crear-prestamo 01 01 01 "28/01" 5)) 
;(obtener-fecha-vencimiento p1)

