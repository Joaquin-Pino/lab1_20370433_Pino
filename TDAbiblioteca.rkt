#lang racket

(require "utilidades.rkt")
(require "TDAlibro.rkt")
(require "TDAusuario.rkt")

; definicion
(define (crear-biblioteca libros usuarios prestamos max-libros dias-max
                          tasa-multa limite-deuda dias-retraso fecha-inicial) ;RF05
  (list libros usuarios prestamos max-libros dias-max
        tasa-multa limite-deuda dias-retraso fecha-inicial))

;selectores
(define (libro-en-biblioteca? biblioteca libro); me quivoque XD, redundatne con lo que hace rf09
  (let ((lista-libros (libros-en-biblioteca biblioteca))
        (id-libro-ingresar (get-libro-id libro)))
    (define (buscar-id lista-lib)
      (cond
        ((null? lista-lib) #f)
        ((= (get-libro-id (car lista-lib)) id-libro-ingresar) #t)
        (else (buscar-id (cdr lista-lib)))))
    (buscar-id lista-libros)))

(define (libros-en-biblioteca biblioteca) (obtener-dato biblioteca 0))
(define (usuarios-biblioteca biblioteca) (obtener-dato biblioteca 1))
(define (obtener-prestamos biblioteca) (obtener-dato biblioteca 2))
(define (obtener-max-libros biblioteca) (obtener-dato biblioteca 3))
(define (obtener-max-dias-prestamo biblioteca) (obtener-dato biblioteca 4))
(define (obtener-tasa-multa biblioteca) (obtener-dato biblioteca 5))
(define (obtener-limite-deuda biblioteca) (obtener-dato biblioteca 6))
(define (obtener-biblioteca-dias-retraso biblioteca) (obtener-dato biblioteca 7))
(define (obtener-fecha-actual biblioteca) (obtener-dato biblioteca 8))

;modificadores
(define (agregar-libro biblioteca libro) ;RF06
  (if (not (libro-en-biblioteca? biblioteca libro))
      (let ((lista-libros (libros-en-biblioteca biblioteca)))
        (let ((nuevos-libros (agregar-inicio-lista libro lista-libros))); se agrega libro al inicio
          (modificar-elemento biblioteca 0 nuevos-libros))) ;se entrega la biblioteca modificada
      biblioteca ;; se entrega la biblioteca sin cambios
      ))

;registrar usuario RF07
(define (registrar-usuario biblioteca usuario)
  (define (usuario-presente? lista-usr id-usr)
    (cond
      ((null? lista-usr) #f)
      ((= (id-usuario (car lista-usr)) id-usr) #t)
      (else (usuario-presente? (cdr lista-usr) id-usr))
      )
    )
  
  (let ((id-usr (id-usuario usuario))
        (lista-usuarios (usuarios-biblioteca biblioteca)))
    (if (usuario-presente? lista-usuarios id-usr)
        biblioteca; si el usuario esta presente retornamos biblioteca
        (modificar-elemento biblioteca 1 (agregar-final-lista usuario lista-usuarios))
        )
    )
  )

;pertenencia 
(define (biblioteca? bib)
  (and (list? (libros-en-biblioteca bib))
       (list? (usuarios-biblioteca bib))
       (list? (obtener-prestamos bib))
       (number? (obtener-max-libros bib))
       (number? (obtener-max-dias-prestamo bib)))
  )

;otros
(define (buscar-libro biblio criterio valor)
  (let ((lista-libros (libros-en-biblioteca biblio)))
    (cond
      ((string=? criterio "id")
       
       )
      ((string=? criterio "autor") (display "estamos bsucando por autor") (newline))
      ((string=? criterio "titulo") (display "estamos busacndo por titulo") (newline))
      (else null)
      )
    )
  )

;obtener usuario RF08 
(define (obtener-usuario biblioteca id)
  (let ((lista-usrs (usuarios-biblioteca biblioteca)))
    (define (buscar lista)
      (cond
        ((null? lista) null)
        ((= (id-usuario (car lista)) id) (car lista))
        (else (obtener-usuario (cdr lista) id))
        )
      ) 
    (buscar lista-usrs)
    ) 
  )




;pruebas
;---------------------------------------------------------------------------------
(define biblio (crear-biblioteca null null null 2 3 100 1000 10 "01/01"))
;(display biblio) (newline)
(define lib1 (crear-libro 01 "1984" "jorjor wel"))
(define lib2 (crear-libro 02 "Juramentada" "BRANDON saNDersON"))
(define lib3 (crear-libro 01 "asdf" "asdffa"))

(define b2 (agregar-libro biblio lib1))
;(display b2) (newline)
(define b3 (agregar-libro b2 lib2))
;(display b3) (newline)
(define b4 (agregar-libro b3 lib3))
(display b4) (newline)

(define usr (crear-usuario 01 "asdf"))
(define usr2 (crear-usuario 02 "joaquin"))
(define usr3 (crear-usuario 03 "avbxcvbxcvb"))

(define b5 (registrar-usuario b4 usr))
(define b6 (registrar-usuario b5 usr2))
(define b7 (registrar-usuario b6 usr3))
(display b7) (newline)

(define test (obtener-usuario b6 01))
(display test) (newline)