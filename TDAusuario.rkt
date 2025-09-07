#lang racket

(require "utilidades.rkt")

;DEFINICION DE USUARIO
; crea representacion de un usuario
; dominio: int, str
; recorrido: ususario
(define (crear-usuario id nombre)(list id nombre 0 #f))

;selectores
; obtiene id de un usuario
; dominio: usuario
; recorrido: int
(provide id-usuario)
(define (id-usuario usuario) (obtener-dato usuario 0))

; obtiene nombre de usuario
; domino: usuario
; recorrido: str
(define (nombre-usuario usuario) (obtener-dato usuario 1))

; determina si un usuario esta suspendido
; domino: usuario
; recorrido: bool
(define (usuario-suspendido? usuario) (obtener-dato usuario 3)) ; RF13

; obtiene deuda de usuario
; dominio: usuario
; recorrido: int
(define (obtener-deuda usuario) (obtener-dato usuario 2)) ;RF14

;modificadores
; 

;pertenencia
; determina si dato es del tipo usuario
; dominio: cualquier tipo de dato
; recorrido: bool
(define (usuario? usuario)
  (and (number? (id-usuario usuario))
       (string? (nombre-usuario usuario))
       (number? (obtener-deuda usuario))
       (boolean? (usuario-suspendido? usuario))
       (= (length usuario) 4)
       ))

;(define usr (crear-usuario 01 "joaquin"))
;(usuario? usr)