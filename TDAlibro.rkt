#lang racket

(require "utilidades.rkt")

; -----------------------------definicion-----------------------------
; crea la representacion de un libro
; dominino: int, str, str
; recorrido: libro
(provide crear-libro)
(define (crear-libro id titulo autor) (list id (string-downcase titulo) (string-downcase autor)))

;-----------------------------selectores-----------------------------
; obtiene id de un libro
; dominio: libro
; recorrido: int
(provide get-libro-id)
(define (get-libro-id libro) (obtener-dato libro 0))

; obtiene titulo de un libro
; dominio: libro
; recorrido: str
(define (get-libro-titulo libro)  (obtener-dato libro 1))

; obtiene titulo de un libro
; dominio: libro
; recorrido: str
(define (get-libro-autor libro) (obtener-dato libro 2))

;-----------------------------pertenencia-----------------------------
; valida que dato prestenence a la estructura indicada
; dominio: cualquier tipo de dato
; recorrido: bool
(define (libro? libro)
  (and (number? (get-libro-id libro))
       (string? (get-libro-titulo libro))
       (string? (get-libro-autor libro))
       (= (length libro) 3))
  )