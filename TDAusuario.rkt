#lang racket

(require "utilidades.rkt")

;-----------------------------DEFINICION DE USUARIO-----------------------------
; crea representacion de un usuario
; dominio: int, str
; recorrido: ususario
(provide crear-usuario); id , nombre, deuda, suspendido
(define (crear-usuario id nombre) (list id nombre 0 #f))

;-----------------------------selectores-----------------------------
; obtiene id de un usuario
; dominio: usuario
; recorrido: int
(provide id-usuario)
(define (id-usuario usuario) (list-ref usuario 0))

; obtiene nombre de usuario
; domino: usuario
; recorrido: str
(define (nombre-usuario usuario) (list-ref usuario 1))

; obtiene deuda de usuario
; dominio: usuario
; recorrido: int
(provide obtener-deuda)
(define (obtener-deuda usuario) (list-ref usuario 2)) ;RF14

; determina si un usuario esta suspendido
; domino: usuario
; recorrido: bool
(provide usuario-suspendido?)
(define (usuario-suspendido? usuario) (list-ref usuario 3)) ; RF13

;-----------------------------modificadores-----------------------------
(provide suspender)
(define (suspender usr)
  (map (lambda (campo)
         (if (eq? campo #f)
            #t
            campo)
         ) usr))

(provide modificar-usuario-deuda-estado)
(define (modificar-usuario-deuda-estado usuario nueva-deuda nuevo-estado)
  (list (id-usuario usuario) (nombre-usuario usuario) nueva-deuda nuevo-estado))
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
