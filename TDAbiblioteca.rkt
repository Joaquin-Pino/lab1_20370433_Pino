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
(provide crear-biblioteca)
(define (crear-biblioteca libros usuarios prestamos max-libros dias-max
                          tasa-multa limite-deuda dias-retraso fecha-inicial ) ;RF05
  (list libros usuarios prestamos max-libros dias-max
        tasa-multa limite-deuda dias-retraso fecha-inicial  ))

;-----------------------------selectores-----------------------------
;obtiene libros en la biblioteca, entrega una lista
; dominio: biblioteca
; recorrido: lista libros
(provide libros-en-biblioteca)
(define (libros-en-biblioteca biblioteca) (obtener-dato biblioteca 0))

;obtiene usuarios en la biblioteca, entrega lista
; dominio: biblioteca
; recorrido: lista usarios
(provide usuarios-biblioteca)
(define (usuarios-biblioteca biblioteca) (obtener-dato biblioteca 1))

;obtiene prestamos (historial) en la biblioteca, entrega lista
; dominio: biblioteca
; recorrido: lista prestamos
(provide obtener-prestamos)
(define (obtener-prestamos biblioteca) (obtener-dato biblioteca 2)) ;historial de prestamos

;obtiene cantidad maxima de libros que puede tener un usuario
;dominio: biblioteca
;recorrido: int
(provide obtener-max-libros)
(define (obtener-max-libros biblioteca) (obtener-dato biblioteca 3))

;obtiene duracion maxima de un prestamo
;dominio: biblioteca
;recorrido: int
(provide obtener-max-dias-prestamo)
(define (obtener-max-dias-prestamo biblioteca) (obtener-dato biblioteca 4))

;obtiene tasa de multa por atraso
;dominio: biblioteca
;recorrido: int
(provide obtener-tasa-multa)
(define (obtener-tasa-multa biblioteca) (obtener-dato biblioteca 5))

;obtiene limite de deuda de la biblioteca
;dominio: biblioteca
;recorrido: int
(provide obtener-limite-deuda)
(define (obtener-limite-deuda biblioteca) (obtener-dato biblioteca 6))

;obtiene dias de retraso maximo antes de suspension de usuario
;dominio: biblioteca
;recorrido: int
(provide obtener-biblioteca-dias-retraso)
(define (obtener-biblioteca-dias-retraso biblioteca) (obtener-dato biblioteca 7))

;;obtiene la fecha actual de la biblioteca
;dominio: biblioteca
;recorrido: string
(provide get-fecha)
(define (get-fecha biblioteca) (obtener-dato biblioteca 8))

;verifica si esta prestente un usuario en la biblioteca
;dominio: biblioteca, int
;recorrido: bool
(provide usuario-presente?)
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
(provide libro-en-biblioteca?)
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
(provide obtener-usuario)
(define (obtener-usuario biblioteca id) ; RF08
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
(provide agregar-libro)
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
(provide registrar-usuario)
(define (registrar-usuario biblioteca usuario); RF07
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
;recorrido: libro, null
(provide buscar-libro)
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
(provide calcular-dias-retraso)
(define (calcular-dias-retraso fecha-actual fecha-vencimiento)
  (let ((vencimiento (leer-fecha fecha-vencimiento))
        (actual (leer-fecha fecha-actual)))

    (if (<= (diferencia-dias actual vencimiento) 0)
        0
        (diferencia-dias actual vencimiento)
        )))


;calcula multa por dias de atraso
;dominio: prestamo, str, str
;recorrido: int
(provide calcular-multa)
(define (calcular-multa prestamo fecha-actual tasa-multa) ;RF17
  (let ((fecha-vencimiento (obtener-fecha-vencimiento prestamo)))
    (let ((dias-atraso (calcular-dias-retraso fecha-actual fecha-vencimiento)))
      (*  dias-atraso tasa-multa))
    ))

(define (historial-usr biblioteca id-usr) ;aux
  (let ((historial-biblioteca (obtener-prestamos biblioteca)))
    (filter (lambda (prestamo)
              (= (id-usuario-prestamo prestamo) id-usr)) historial-biblioteca)
    ))

(define (print-prestamo prestamo) ; aux
  (let ((estado-str (if (get-estado-prestamo prestamo) "Activo" "Completado"))
        )
    (string-append
     "Préstamo #"
     (number->string (id-prestamo prestamo))
     " - Libro "
     (number->string (id-libro-prestado prestamo))
     " - Prestado: "
     (fecha-prestamo prestamo)
     " - Vence: "
     (obtener-fecha-vencimiento prestamo)
     " "
     estado-str
     "\n")))

(provide historial-prestamos-usuario)
(define (historial-prestamos-usuario biblioteca id-usr) ;RF24
  (let ((usuario (obtener-usuario biblioteca id-usr)))
    (if (not usuario)
        "" ;string vacia
        (let ((historial-completo (obtener-prestamos biblioteca)))
          (let ((historial-usuario (historial-usr biblioteca id-usr)))
            (if (null? historial-usuario)
                "";string vacia
                (map print-prestamo historial-usuario)))))))


(define (hay-atraso? historial-prestamo fecha-actual) ;aux
  (let ((prestamos-activos (filter (lambda (prestamo)
                                     (get-estado-prestamo prestamo))
                                   historial-prestamo)))
    (let ((prestamos-atrasados (filter (lambda (prestamo)
                                         (> (calcular-dias-retraso fecha-actual (obtener-fecha-vencimiento prestamo)) 0))
                                       prestamos-activos)))
      (if (null? prestamos-atrasados) #f #t)
      )))

(provide debe-suspenderse?)
(define (debe-suspenderse? biblioteca id-usr fecha-actual);RF20
  (let ((historial-usuario (historial-usr biblioteca id-usr))
        (limite-maximo (obtener-limite-deuda biblioteca)))
    (let ((prestamos-activos (filter (lambda (prestamo)
                                       (eq? (get-estado-prestamo prestamo) #t)) historial-usuario)))

      (if (or (> (length prestamos-activos) limite-maximo) (hay-atraso? historial-usuario fecha-actual) )
          #t
          #f)
      )))

(provide libro-disponible?)
(define (libro-disponible? biblioteca id-libro) ;RF12
  (let ((libro (buscar-libro biblioteca "id" id-libro))
        (lista-prestamos (obtener-prestamos biblioteca)))
    (let ((lista-prestamos-libro (filter (lambda (prestamo)
                                           (= id-libro (id-libro-prestado prestamo))) lista-prestamos)))
      (cond
        ((not (libro? libro)) #f)

        ((null? (filter (lambda (prestamo)
                          (eq? (get-estado-prestamo prestamo) #t)) lista-prestamos-libro)) #t)

        (else #f)
        ))))

(define (crear-id-prestamo biblioteca) ;aux
  (let ((lista-prestamos (obtener-prestamos biblioteca)))
    (if (null? lista-prestamos)
        01
        (let ((lista-ids (map id-prestamo lista-prestamos)))
          (+ (apply max lista-ids) 1))))
  )

(provide tomar-prestamo)
(define (tomar-prestamo biblioteca id-usr id-libro dias-solicitados fecha-actual) ;RF18
  ; podria mover todos estos let, menos el de usuario despues del if, pero ya funciona laksdjhfa
  (let ((libro-disp? (libro-disponible? biblioteca id-libro))
        (dias-max-prestamo (obtener-max-dias-prestamo biblioteca))
        (limite-deuda (obtener-limite-deuda biblioteca))
        (libros-max (obtener-max-libros biblioteca))
        (libros-usr (historial-usr biblioteca id-usr)))
    ;se asume que usuario ingresado esta registrado en la biblioteca
    (if (usuario-suspendido? (obtener-usuario biblioteca id-usr)) ;aca hay composicion
        biblioteca
        (let ((deuda-usr (obtener-deuda (obtener-usuario biblioteca id-usr)))) ;composicion 
          (if (and libro-disp?
                   (<= dias-solicitados dias-max-prestamo) 
                   (not (usuario-suspendido? (obtener-usuario biblioteca id-usr))) ;composicion
                   (< deuda-usr limite-deuda) 
                   (< (length libros-usr) libros-max))
              
              (let ((id-nuevo-prestamo (crear-id-prestamo biblioteca))
                    (lista-antigua-prestamos (obtener-prestamos biblioteca)))
                (let ((nuevo-prestamo (crear-prestamo id-nuevo-prestamo id-usr id-libro fecha-actual dias-solicitados)))
                  (let ((nueva-lista-prestamos (agregar-inicio-lista nuevo-prestamo lista-antigua-prestamos)))
                    (crear-biblioteca (libros-en-biblioteca biblioteca) 
                                      (usuarios-biblioteca biblioteca)
                                      nueva-lista-prestamos
                                      (obtener-max-libros biblioteca)
                                      (obtener-max-dias-prestamo biblioteca)
                                      (obtener-tasa-multa biblioteca)
                                      (obtener-limite-deuda biblioteca)
                                      (obtener-biblioteca-dias-retraso biblioteca)
                                      (get-fecha biblioteca)))))
              
              
              biblioteca)
          ))))

(provide suspender-usuario)
(define (suspender-usuario biblioteca id-usr) ;rf20
  (let ((usuario (obtener-usuario biblioteca id-usr)))
    (if (usuario-suspendido? usuario)
        biblioteca
        (let ((usuario-suspendido (suspender usuario))
              (lista-usuarios (usuarios-biblioteca biblioteca)))

          (let ((nueva-lista-usuarios 
                 (map (lambda (usuario-actual)
                        (if (= (id-usuario usuario-actual) id-usr)
                            usuario-suspendido
                            usuario-actual))
                      lista-usuarios)))

            (crear-biblioteca (libros-en-biblioteca biblioteca)
                              nueva-lista-usuarios
                              (obtener-prestamos biblioteca)
                              (obtener-max-libros biblioteca)
                              (obtener-max-dias-prestamo biblioteca)
                              (obtener-tasa-multa biblioteca)
                              (obtener-limite-deuda biblioteca)
                              (obtener-biblioteca-dias-retraso biblioteca)
                              (get-fecha biblioteca))
            )))))

;obtiene prestamo por id
(define (obtener-prestamo biblioteca id-prest) ;aux
  (let ((lista-prestamos (obtener-prestamos biblioteca)))
    (let ((resultado (filter (lambda (prestamo)
                               (= id-prest (id-prestamo prestamo))) lista-prestamos)))
      (if (null? resultado) 
          '()
          (car resultado)
          ))))

(provide renovar-prestamo)
(define renovar-prestamo ;probar
  (lambda (biblioteca)
    (lambda(id-prest)
      (lambda(dias-extra fecha-actual)

        (let ((prestamo (obtener-prestamo biblioteca id-prest)))
          (if (null? prestamo)
              biblioteca
              (let ((estado-prestamo (get-estado-prestamo prestamo)) ; estado-prestamo: activo->#t, completado->#f
                    (usuario (obtener-usuario biblioteca (id-usuario-prestamo prestamo)))
                    (retraso (calcular-dias-retraso fecha-actual (obtener-fecha-vencimiento prestamo)))
                    (excede-max? (> (+ dias-extra (duracion-prestamo prestamo)) (obtener-max-dias-prestamo biblioteca)))
                    (lista-prestamos (obtener-prestamos biblioteca))
                    )
                (if (and estado-prestamo (not (usuario-suspendido? usuario)) (<= retraso 0) (not excede-max?))
                    ;se extiende prestamo
                    (map (lambda (p)
                           (if (= id-prest (id-prestamo p)) ; se cambia la duracion del prestamo si hay ids iguales
                               (cambiar-duracion-prestamo p (+ dias-extra (duracion-prestamo prestamo))) 
                               p
                               )) lista-prestamos)
                    biblioteca ; no se cumplio alguna condicion y se devuelve biblioteca sin cambios
                    )
                )))))))


(provide devolver-libro)
(define (devolver-libro biblioteca id-usr id-libro fecha-actual) ;rf19
  (let ((prestamo (filter (lambda (p) ;obtenemos prestamo, pero esta como lista
                            (and (= (id-libro-prestado p) id-libro)
                                 (= (id-usuario-prestamo p) id-usr)
                                 (get-estado-prestamo p)))
                          (obtener-prestamos biblioteca))))
    
    (if (null? prestamo)
        biblioteca ; si no se encuentra prestamo, se devuelve la biblioteca sin cambios

        ; let se usa para manejar todo lo necesario con el prestamo
        (let ((usuario (obtener-usuario biblioteca id-usr))
              (prest (car prestamo)))
          (let ((multa (calcular-multa prest fecha-actual (obtener-tasa-multa biblioteca)))
                (limite-deuda (obtener-limite-deuda biblioteca))
                (historial-usuario (historial-usr biblioteca id-usr)))
            (let ((nueva-deuda (+ (obtener-deuda usuario) multa)))
              (let ((suspender? (or (>= nueva-deuda limite-deuda) (hay-atraso? historial-usuario fecha-actual))))
                
                (let ((usuario-actualizado (modificar-usuario-deuda-estado usuario nueva-deuda suspender?))
                      (prestamo-actualizado (modificar-estado-prestamo prest #f)))
                  (let ((nueva-lista-usuarios (map (lambda (u)
                                                     (if (= (id-usuario u) id-usr)
                                                         usuario-actualizado
                                                         u))
                                                   (usuarios-biblioteca biblioteca)))
                        
                        (nueva-lista-prestamos (map (lambda (p)
                                                      (if (= (id-prestamo p) (id-prestamo prest))
                                                          prestamo-actualizado
                                                          p))
                                                    (obtener-prestamos biblioteca))))

                    ; se devuleve biblioteca actualizada
                    (crear-biblioteca (libros-en-biblioteca biblioteca)
                                      nueva-lista-usuarios
                                      nueva-lista-prestamos
                                      (obtener-max-libros biblioteca)
                                      (obtener-max-dias-prestamo biblioteca)
                                      (obtener-tasa-multa biblioteca)
                                      (obtener-limite-deuda biblioteca)
                                      (obtener-biblioteca-dias-retraso biblioteca)
                                      (get-fecha biblioteca))

                    ))))))))) 


(provide pagar-deuda)
(define (pagar-deuda biblioteca id-usr monto)
  (let ((usuario (obtener-usuario biblioteca id-usr))
        (historial-usuario (historial-usr biblioteca id-usr))
        (fecha-actual (get-fecha biblioteca)))
    (let ((deuda-usr (obtener-deuda usuario))
          (retraso? (hay-atraso? historial-usuario fecha-actual)))
      
      (let ((nueva-deuda (if (< (- deuda-usr monto) 0) 0 (- deuda-usr monto)))) 
        ;(si no hay retraso y deuda 0) -> not #t (no suspendido)
        (let ((nuevo-estado-usr (not (and (= nueva-deuda 0) (not retraso?)))))
          (let ((usr-actualizado (modificar-usuario-deuda-estado usuario nueva-deuda nuevo-estado-usr)))

            (let ((nueva-lista-usuarios (map (lambda (u)
                                               (if (= (id-usuario u) id-usr)
                                                   usr-actualizado
                                                   u
                                                   )) (usuarios-biblioteca biblioteca))))

              (crear-biblioteca (libros-en-biblioteca biblioteca)
                                nueva-lista-usuarios
                                (obtener-prestamos biblioteca)
                                (obtener-max-libros biblioteca)
                                (obtener-max-dias-prestamo biblioteca)
                                (obtener-tasa-multa biblioteca)
                                (obtener-limite-deuda biblioteca)
                                (obtener-biblioteca-dias-retraso biblioteca)
                                (get-fecha biblioteca))
              ))))))) 

; recibe un usuario y devuelve su estado actualizado para el día siguiente.
(define (usuario-nuevo-dia usuario biblioteca nueva-fecha-sistema) ;aux
  (let ((id-usr (id-usuario usuario))
        (tasa-multa (obtener-tasa-multa biblioteca))
        (lista-prestamos (obtener-prestamos biblioteca)))
   
    (let ((prestamos-activos-usr (filter (lambda (p)
                                           (and (= (id-usuario-prestamo p) id-usr)
                                                (get-estado-prestamo p)))
                                         lista-prestamos)))
      (let ((lista-de-multas (map (lambda (p) (calcular-multa p nueva-fecha-sistema tasa-multa))
                                  prestamos-activos-usr)))
        (let ((multa-total-del-dia (apply + lista-de-multas)))
          (let ((nueva-deuda  multa-total-del-dia))

            (let ((nuevo-estado-susp (or (usuario-suspendido? usuario)
                                         (debe-suspenderse? biblioteca id-usr nueva-fecha-sistema))))
              
              (modificar-usuario-deuda-estado usuario nueva-deuda nuevo-estado-susp))))))))

(provide procesar-dia)
(define (procesar-dia biblioteca)
  (let ((nueva-fecha-sistema (fecha->string (sumar-dias (leer-fecha (get-fecha biblioteca)) 1))))
    (let ((nueva-lista-usuarios (map (lambda (usr) ;para cada usuario en la lista, 
                                       (usuario-nuevo-dia usr biblioteca nueva-fecha-sistema))
                                     (usuarios-biblioteca biblioteca))))
      (crear-biblioteca (libros-en-biblioteca biblioteca)
                        nueva-lista-usuarios
                        (obtener-prestamos biblioteca)
                        (obtener-max-libros biblioteca)
                        (obtener-max-dias-prestamo biblioteca)
                        (obtener-tasa-multa biblioteca)
                        (obtener-limite-deuda biblioteca)
                        (obtener-biblioteca-dias-retraso biblioteca)
                        nueva-fecha-sistema))))
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

(define prestamo1 (crear-prestamo 01 1 101 "01/09" 5))
(define prestamo2 (crear-prestamo 02 1 103 "01/09" 3))

(print-prestamo prestamo2)
(define b8 (suspender-usuario b7 01))

(display (obtener-usuario b8 01))