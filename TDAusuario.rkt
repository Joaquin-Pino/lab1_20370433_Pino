#lang racket

(require "utilidades.rkt")

; DEFINICION DE USUARIO
(define (crear-usuario id nombre)(list id nombre 0 #f))


;selectores
(define (id-usuario usuario) (list-ref usuario 0))
(define (nombre-usuario usuario) (list-ref usuario 1))
(define (usuario-suspendido? usuario) (list-ref usuario 3)) ; RF13
(define (obtener-deuda usuario) (list-ref usuario 2)) ;RF14

;modificadores
(define (cambiar-nombre usuario nuevo)
  (modificar-elemento usuario 1 nuevo))

;pertenencia
(usuario-suspendido? (crear-usuario 1 "asdrf"))
