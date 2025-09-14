#lang racket

(require "utilidades.rkt")
(require "TDAlibro.rkt")
(require "TDAusuario.rkt")
(require "TDAdia.rkt")
(require "TDAprestamo.rkt")

; -----------------------------definicion-----------------------------
;crea una biblioteca
; dominio: libro, usuario, prestamo, int, int, int, int, int, string
; recorrido: biblioteca
(define (crear-biblioteca libros usuarios prestamos max-libros dias-max
                          tasa-multa limite-deuda dias-retraso fecha-inicial ) ;RF05
  (list libros usuarios prestamos max-libros dias-max
        tasa-multa limite-deuda dias-retraso fecha-inicial  ))

;-----------------------------selectores-----------------------------
;obtiene libros en la biblioteca, entrega una lista
; dominio: biblioteca
; recorrido: lista libros
(define (libros-en-biblioteca biblioteca) (obtener-dato biblioteca 0))

;obtiene usuarios en la biblioteca, entrega lista
; dominio: biblioteca
; recorrido: lista usarios
(define (usuarios-biblioteca biblioteca) (obtener-dato biblioteca 1))

;obtiene prestamos (historial) en la biblioteca, entrega lista
; dominio: biblioteca
; recorrido: lista prestamos
(define (obtener-prestamos biblioteca) (obtener-dato biblioteca 2)) ;historial de prestamos

;obtiene cantidad maxima de libros que puede tener un usuario
;dominio: biblioteca
;recorrido: int
(define (obtener-max-libros biblioteca) (obtener-dato biblioteca 3))

;obtiene duracion maxima de un prestamo
;dominio: biblioteca
;recorrido: int
(define (obtener-max-dias-prestamo biblioteca) (obtener-dato biblioteca 4))

;obtiene tasa de multa por atraso
;dominio: biblioteca
;recorrido: int
(define (obtener-tasa-multa biblioteca) (obtener-dato biblioteca 5))

;obtiene limite de deuda de la biblioteca
;dominio: biblioteca
;recorrido: int
(define (obtener-limite-deuda biblioteca) (obtener-dato biblioteca 6))

;obtiene dias de retraso maximo antes de suspension de usuario
;dominio: biblioteca
;recorrido: int
(define (obtener-biblioteca-dias-retraso biblioteca) (obtener-dato biblioteca 7))

;;obtiene la fecha actual de la biblioteca
;dominio: biblioteca
;recorrido: string
(define (get-fecha biblioteca) (obtener-dato biblioteca 8))

;verifica si esta prestente un usuario en la biblioteca
;dominio: biblioteca, int
;recorrido: bool
(define (usuario-presente? biblioteca id-usr)
  (let ((lista-usr (usuarios-biblioteca biblioteca))) 
    (define (aux lista)
      (cond 
        ((null? lista) #f)
        ((= (id-usuario (car lista)) id-usr) #t)
        (else (aux (cdr lista)))))     
    (aux lista-usr)))

;verifica que libro este presnete en la biblioteca 
;dominio: biblioteca, int
;recorrido: bool
(define (libro-en-biblioteca? biblioteca libro)
  (let ((lista-libros (libros-en-biblioteca biblioteca))
        (id-libro-ingresar (get-libro-id libro)))
    (define (buscar-id lista-lib)
      (cond
        ((null? lista-lib) #f)
        ((= (get-libro-id (car lista-lib)) id-libro-ingresar) #t)
        (else (buscar-id (cdr lista-lib)))))
    (buscar-id lista-libros)))



;obtiene usuario de la bibliioteca, retorna null si no se encuentra
;dominio: biblioteca, int
;recorrido: usuario, null
(define (obtener-usuario biblioteca id)
  ;obtener usuario RF08
  (let ((lst-usrs (usuarios-biblioteca biblioteca)))
    (define (aux lst)
      (cond
        ((null? lst) '())
        ((= (id-usuario (car lst)) id) (car lst))
        (else
         (aux (cdr lst)))))
    (aux lst-usrs)))
      
;-----------------------------modificadores-----------------------------
;agrega libro a la lista de libros de la biblioteca, si libro existe, se entrega biblioteca sin cambios
;dominio: biblioteca, libro
;recorrido: bibilioteca
(define (agregar-libro biblioteca libro) ;RF06
  (if (not (libro-en-biblioteca? biblioteca libro))
      (let ((lista-libros (libros-en-biblioteca biblioteca)))
        (let ((nuevos-libros (agregar-inicio-lista libro lista-libros))); se agrega libro al inicio
          (crear-biblioteca nuevos-libros (usuarios-biblioteca biblioteca)
                            (obtener-prestamos biblioteca)
                            (obtener-max-libros biblioteca)
                            (obtener-max-dias-prestamo biblioteca)
                            (obtener-tasa-multa biblioteca)
                            (obtener-limite-deuda biblioteca)
                            (obtener-biblioteca-dias-retraso biblioteca)
                            (get-fecha biblioteca)
                            ))) ;se entrega la biblioteca modificada
      biblioteca ;; se entrega la biblioteca sin cambios
      ))


;registra usuario en la biblioteca, si existe usuario se entrega biblioteca sin cambios
;dominio: biblioteca, usuario
;recorrido: biblioteca
(define (registrar-usuario biblioteca usuario)
  ;registrar usuario RF07
  (let ((id-usr (id-usuario usuario)) 
        (lista-usuarios (usuarios-biblioteca biblioteca)))
    (if (usuario-presente? biblioteca id-usr)
        biblioteca; si el usuario esta presente retornamos biblioteca
        (let ((lista-con-nuevo-usuario (agregar-final-lista usuario lista-usuarios)))
          (crear-biblioteca (libros-en-biblioteca biblioteca) lista-con-nuevo-usuario
                            (obtener-prestamos biblioteca)
                            (obtener-max-libros biblioteca)
                            (obtener-max-dias-prestamo biblioteca)
                            (obtener-tasa-multa biblioteca)
                            (obtener-limite-deuda biblioteca)
                            (obtener-biblioteca-dias-retraso biblioteca)
                            (get-fecha biblioteca)
                            )))))

;-----------------------------pertenencia-----------------------------
;comprueba que tipo de dato sea perteneciente al tipo biblioteca
;dominio: cualquier tipo de dato
;recorrido: bool
(define (biblioteca? bib)
  (and (list? (libros-en-biblioteca bib))
       (list? (usuarios-biblioteca bib))
       (list? (obtener-prestamos bib))
       (number? (obtener-max-libros bib))
       (number? (obtener-max-dias-prestamo bib)))
  )

;-----------------------------otros-----------------------------

;busca libro por id, autor o titoulo, devuelve null si no se encuentra
;dominio: biblioteca (int o string)
;recorrido: biblioteca
(define (buscar-libro biblio criterio valor)
  (let ((lista-libros (libros-en-biblioteca biblio)))
    (let ((resultado
           (cond
             ((string=? criterio "id")
              (filter (lambda (libro)
                        (= (get-libro-id libro) valor))
                      lista-libros))
             ((string=? criterio "autor")
              (filter (lambda (libro)
                        (string-contains? (get-libro-autor libro) (string-downcase valor)))
                      lista-libros))
             ((string=? criterio "titulo")
              (filter (lambda (libro)
                        (string-contains? (get-libro-titulo libro) (string-downcase valor)))
                      lista-libros))
             (else '())))) ; no se encontro el libro
      
      (if (null? resultado)
          '()
          (car resultado)))))
 
;----------------------------- otros -----------------------------
;calcula dias de atraso, retorna 0 si no hay atraso
; dominio: str, str
; recorrido: int
(define (calcular-dias-retraso fecha-actual fecha-vencimiento)
  (let ((vencimiento (leer-fecha fecha-vencimiento))
        (actual (leer-fecha fecha-actual)))

    (if (<= (diferencia-dias actual vencimiento) 0)
        0
        (diferencia-dias actual vencimiento)
        )
    ))


;calcula multa por dias de atraso
;dominio: prestamo, str, str
;recorrido: int 
(define (calcular-multa prestamo fecha-actual tasa-multa)
  (let ((fecha-vencimiento (obtener-fecha-vencimiento prestamo)))
    (let ((dias-atraso (calcular-dias-retraso fecha-actual fecha-vencimiento)))
      (*  dias-atraso tasa-multa))
    )
  )

#|
;(define (hay-atraso? usuario historial-prestamo))

(define (debe-suspenderse? biblioteca id-usr fecha-actual)
  (let ((usuario (obtener-usuario biblioteca id-usr))
        (limite-max (obtener-limite-deuda biblioteca))))
  ;; hacer otro let con el historial de prestamo del usuario y verificar si hay alugn atraso
  )

(define (suspender-usuario biblioteca id-usr)
  (let ((usr (obtener-usuario biblioteca id-usr)))
    (cond
      ((usuario? usr) (if (debe-suspenderse? biblioteca id-usr (get-fecha biblioteca))
                          ;;crear nueva biblioteca
                          biblioteca

                          )
                      )
      )

    ))

|#

#|(define (libro-disponible? biblioteca id-libro)
  (let ((libro (buscar-libro biblioteca "id" valor))))
  
  )
|#
;debe usar composicion de funciones
;(define (tomar-prestamo biblioteca  id-usr id-libro dias-solicitados fecha-actual))

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


(calcular-dias-retraso "04/01" "04/02" )

(define prest (crear-prestamo 01 01 01 "01/01" 3))
(obtener-fecha-vencimiento prest)

(calcular-multa prest "07/01" 100)
(buscar-libro b4 "autor" "jorjor")