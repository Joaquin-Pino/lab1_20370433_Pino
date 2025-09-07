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
(define (obtener-prestamos biblioteca) (obtener-dato 2))
(define (obtener-max-libros biblioteca) (obtener-dato 3))
(define (obtener-max-dias-prestamo biblioteca) (obtener-dato 4))
(define (obtener-tasa-multa biblioteca) (obtener-dato 5))
(define (obtener-limite-deuda biblioteca) (obtener-dato 6))
(define (obtener-biblioteca-dias-retraso biblioteca) (obtener-dato 7))
(define (obtener-fecha-actual biblioteca) (obtener-dato 8))


;modificadores
(define (agregar-libro biblioteca libro) ;RF06
  (if (not (libro-en-biblioteca? biblioteca libro))
      (let ((lista-libros (libros-en-biblioteca biblioteca)))
        (let ((nuevos-libros (agregar libro lista-libros))); se agrega libro al inicio
          (modificar-elemento biblioteca 0 nuevos-libros))) ;se entrega la biblioteca modificada
      biblioteca ;; se entrega la biblioteca sin cambios
      ))

; registrar usuario

;pertenencia
(define (biblioteca? bib)
  (and (list? (libros-en-biblioteca bib))
       (list? (usuarios-biblioteca bib))
       (list? (obtener-prestamos bib))
       (number? (obtener-max-libros bib))
       (number? (obtener-max-dias-prestamo bib))
       ))

;otros
;(define (existe-usuario? biblioteca usuario))


(define (buscar-libro biblio criterio valor)
  (let ((lista-libros (libros-en-biblioteca biblio)))
    (cond
      ((string=? criterio "id")
       ()
       
       )
      ((string=? criterio "autor") (display "estamos bsucando por autor") (newline))
      ((string=? criterio "titulo") (display "estamos busacndo por titulo") (newline))
      (else null)
      )
    )
  )

;pruebas
;---------------------------------------------------------------------------------
(define biblio (crear-biblioteca null null null 2 3 100 1000 10 "01/01"))
(display biblio) (newline)
(define lib1 (crear-libro 01 "1984" "jorjor wel"))
(define lib2 (crear-libro 02 "Juramentada" "BRANDON saNDersON"))
(define lib3 (crear-libro 01 "asdf" "asdffa"))


(define b2 (agregar-libro biblio lib1))
(display b2) (newline)
(define b3 (agregar-libro b2 lib2))
(display b3) (newline)
(define b4 (agregar-libro b3 lib3))
(display b4) (newline)


(buscar-libro b4 "t" 01)

;(display (libro-en-biblioteca? b3 lib3))
