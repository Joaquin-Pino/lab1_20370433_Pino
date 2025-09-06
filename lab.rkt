#lang racket



(define usuario (crear-usuario 123 "joaquin"))

(usuario-suspendido? usuario)

(define lb1 (crear-libro 100 "asdf" "qpwoeiru"))
(get-libro-id lb1)