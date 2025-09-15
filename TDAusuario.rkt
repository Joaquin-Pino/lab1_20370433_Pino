#lang racket

(require "utilidades.rkt")

;-----------------------------DEFINICION DE USUARIO-----------------------------
; crea representacion de un usuario
; dominio: int, str
; recorrido: ususario
(provide crear-usuario); id , nombre, deuda, suspendido, cantidad de libros
(define (crear-usuario id nombre) (list id nombre 0 #f '()))

;-----------------------------selectores-----------------------------
; obtiene id de un usuario
; dominio: usuario
; recorrido: int
(provide id-usuario)
(define (id-usuario usuario) (obtener-dato usuario 0))

; obtiene nombre de usuario
; domino: usuario
; recorrido: str
(define (nombre-usuario usuario) (obtener-dato usuario 1))

; obtiene deuda de usuario
; dominio: usuario
; recorrido: int
(provide obtener-deuda)
(define (obtener-deuda usuario) (obtener-dato usuario 2)) ;RF14

; determina si un usuario esta suspendido
; domino: usuario
; recorrido: bool
(provide usuario-suspendido?)
(define (usuario-suspendido? usuario) (obtener-dato usuario 3)) ; RF13

; obtiene lista de prestamos
; dominio: usuario
; recorrido: lista de prestamos
(provide get-prestamos-usr)
(define (get-prestamos-usr usuario) (obtener-dato usuario 4)) ;RF14

;-----------------------------modificadores-----------------------------
(provide suspender)
(define (suspender usr)
  (crear-usuario (id-usuario usr) (nombre-usuario usr) (obtener-deuda usr) #t (get-prestamos-usr usr)))

;-----------------------------pertenencia-----------------------------
; determina si dato es del tipo usuario
; dominio: cualquier tipo de dato
; recorrido: bool
(provide usuario?)
(define (usuario? usuario)
  (and (number? (id-usuario usuario))
       (string? (nombre-usuario usuario))
       (number? (obtener-deuda usuario))
       (boolean? (usuario-suspendido? usuario))
       (= (length usuario) 4)
       ))
