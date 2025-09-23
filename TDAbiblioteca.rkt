#lang racket

(require "TDAlibro.rkt")
(require "TDAusuario.rkt")
(require "TDAdia.rkt")
(require "TDAprestamo.rkt")

; -----------------------------definicion-----------------------------
;crea una biblioteca
;dominio: libro, usuario, prestamo, int, int, int, int, int, string
; recorrido: biblioteca
(provide crear-biblioteca)
(define (crear-biblioteca libros usuarios prestamos max-libros dias-max
                          tasa-multa limite-deuda dias-retraso fecha-inicial ) ;RF05
  (list libros usuarios prestamos max-libros dias-max
        tasa-multa limite-deuda dias-retraso fecha-inicial  ))

;-----------------------------selectores-----------------------------
;obtiene libros en la biblioteca, entrega una lista
;dominio: biblioteca
; recorrido: lista libros
(provide libros-en-biblioteca)
(define (libros-en-biblioteca biblioteca) (list-ref biblioteca 0))

;obtiene usuarios en la biblioteca, entrega lista
;dominio: biblioteca
; recorrido: lista usarios
(provide usuarios-biblioteca)
(define (usuarios-biblioteca biblioteca) (list-ref biblioteca 1))

;obtiene prestamos (historial) en la biblioteca, entrega lista
;dominio: biblioteca
; recorrido: lista prestamos
(provide obtener-prestamos)
(define (obtener-prestamos biblioteca) (list-ref biblioteca 2)) ;historial de prestamos

;obtiene cantidad maxima de libros que puede tener un usuario
;dominio: biblioteca
;recorrido: int
(provide obtener-max-libros)
(define (obtener-max-libros biblioteca) (list-ref biblioteca 3))

;obtiene duracion maxima de un prestamo
;dominio: biblioteca
;recorrido: int
(provide obtener-max-dias-prestamo)
(define (obtener-max-dias-prestamo biblioteca) (list-ref biblioteca 4))

;obtiene tasa de multa por atraso
;dominio: biblioteca
;recorrido: int
(provide obtener-tasa-multa)
(define (obtener-tasa-multa biblioteca) (list-ref biblioteca 5))

;obtiene limite de deuda de la biblioteca
;dominio: biblioteca
;recorrido: int
(provide obtener-limite-deuda)
(define (obtener-limite-deuda biblioteca) (list-ref biblioteca 6))

;obtiene dias de retraso maximo antes de suspension de usuario
;dominio: biblioteca
;recorrido: int
(provide obtener-max-retraso-biblioteca)
(define (obtener-max-retraso-biblioteca biblioteca) (list-ref biblioteca 7))

;;obtiene la fecha actual de la biblioteca
;dominio: biblioteca
;recorrido: string
(provide get-fecha)
(define (get-fecha biblioteca) (list-ref biblioteca 8))

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
                            (obtener-max-retraso-biblioteca biblioteca)
                            (get-fecha biblioteca)
                            ))) ;se entrega la biblioteca modificada
      biblioteca ;; se entrega la biblioteca sin cambios
      ))

;registra usuario en la biblioteca, si existe usuario se entrega biblioteca sin cambios
;dominio: biblioteca, usuario
;recorrido: biblioteca
(provide registrar-usuario)
(define (registrar-usuario biblioteca usuario); RF07
  (define (agregar-final-lista elemento lista)
  (cond
    ((null? lista) (agregar-inicio-lista elemento lista))
    (else (agregar-inicio-lista (car lista) (agregar-final-lista elemento (cdr lista)))))
    )
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
                            (obtener-max-retraso-biblioteca biblioteca)
                            (get-fecha biblioteca)
                            )))))

;;----------------------------- auxiliares -----------------------------
(provide agregar-inicio-lista) 
(define (agregar-inicio-lista cosa lista)
  (cons cosa lista))

(provide historial-usr)
;entrega lista con el historial de prestamos del usuario
(define (historial-usr biblioteca id-usr) ;aux
  (let ((historial-biblioteca (obtener-prestamos biblioteca)))
    (filter (lambda (prestamo)
              (= (id-usuario-prestamo prestamo) id-usr)) historial-biblioteca)
    ))

;entrega lista con los prestamos activos del usuario
(define (prestamos-activos-usr biblioteca id-usr)
  (let ((historial-prestamos-biblioteca (obtener-prestamos biblioteca)))
    (filter (lambda (p)
              (and (= (id-usuario-prestamo p) id-usr)
                   (eq? (get-estado-prestamo p) #t)
                   ))historial-prestamos-biblioteca)
    ))

;entrega prestamo en formato string
;dominio: prestamo
;recorrido str
(define (print-prestamo prestamo) ; aux
  (let ((estado-str (if (get-estado-prestamo prestamo) "Activo" "Completado")))
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
;verifica si en lista de prestamo ingresada hay algun atraso en los prestamos activos
;dominio: lista de prestamos, string
(define (hay-atraso? historial-prestamo fecha-actual) ;aux
  ;obtenemos lista de prestamos activos
  (let ((prestamos-activos (filter (lambda (prestamo)
                                     (get-estado-prestamo prestamo))
                                   historial-prestamo)))
    ;todos los prestamos atrasados se guardan en una lista
    (let ((prestamos-atrasados (filter (lambda (prestamo)
                                         (> (calcular-dias-retraso fecha-actual (obtener-fecha-vencimiento prestamo)) 0))
                                       prestamos-activos)))
      (if (null? prestamos-atrasados) #f #t) ; si la lista de prestamos atrasados esta vacia, no hay atraso
      )))

;crea id para un prestamo, busca el prestamo con id mas grande y suma 1 a esa id
;dominio: biblioteca
;recorrido: int
(define (crear-id-prestamo biblioteca) ;aux
  (let ((lista-prestamos (obtener-prestamos biblioteca)))
    (if (null? lista-prestamos)
        01;si no hay prestamos se inicia  la lista de prestamos en 01
        (let ((lista-ids (map id-prestamo lista-prestamos)))
          (+ (apply max lista-ids) 1)))) ; sumamos 1 al id mayor 
  )

; obtiene prestamo por id
;dominio: biblioteca, int
;recorrido: prestamo
(define (obtener-prestamo biblioteca id-prest) ;aux
  (let ((lista-prestamos (obtener-prestamos biblioteca)))
    (let ((resultado (filter (lambda (prestamo)
                               (= id-prest (id-prestamo prestamo))) lista-prestamos)))
      (if (null? resultado) 
          '()
          (car resultado)
          ))))

; recibe un usuario y devuelve su estado actualizado para el día siguiente.
;dominio: usuario, biblioteca, str
;recorrido: usuario
(define (usuario-nuevo-dia usuario biblioteca nueva-fecha-sistema) ;aux
  ;obtenemos datos para procesar el dia
  (let ((id-usr (id-usuario usuario))
        (tasa-multa (obtener-tasa-multa biblioteca))
        (lista-prestamos (obtener-prestamos biblioteca)))
    
    (let ((prestamos-activos-usuario (prestamos-activos-usr biblioteca id-usr)))
      ;calculamos multa por cada libro atrasado y se guarda cada multa en lista
      (let ((lista-de-multas (map (lambda (p) (calcular-multa p nueva-fecha-sistema tasa-multa))
                                  prestamos-activos-usuario)))
        (let ((nueva-deuda (apply + lista-de-multas))) ; se suman cada multa
          (let ((nuevo-estado-susp (or (usuario-suspendido? usuario)
                                       (debe-suspenderse? biblioteca id-usr nueva-fecha-sistema))))
              
            (modificar-usuario-deuda-estado usuario nueva-deuda nuevo-estado-susp)))))))
;----------------------------- otros -----------------------------

;busca libro por id, autor o titoulo, devuelve null si no se encuentra
;dominio: biblioteca (int o string)
;recorrido: libro, null
(provide buscar-libro)
(define (buscar-libro biblio criterio valor);rf09
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
 

;calcula dias de atraso, retorna 0 si no hay atraso
;dominio: str, str
; recorrido: int
(provide libro-disponible?)
(define (libro-disponible? biblioteca id-libro) ;RF12
  (let ((libro (buscar-libro biblioteca "id" id-libro))
        (lista-prestamos (obtener-prestamos biblioteca)))
    (let ((lista-prestamos-libro (filter (lambda (prestamo)
                                           (= id-libro (id-libro-prestado prestamo))) lista-prestamos)))
      (cond
        ((not (libro? libro)) #f)

        ((null? (filter (lambda (prestamo) ; si no hay ningun prestamo marcado como activo, significa que libro esta disponible 
                          (eq? (get-estado-prestamo prestamo) #t)) lista-prestamos-libro)) #t) ;devolvemos diponible

        (else #f)
        ))))
;calcula la dias de atraso a partir de fecha actual y al fecha de vencimiento de un prestamo
;dominio: str, str
;recorrido: int
(provide calcular-dias-retraso)
(define (calcular-dias-retraso fecha-actual fecha-vencimiento);rf16
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

;toma usuario toma prestado libro
;dominio: biblioteca, int, int, int, string
;recorrido: biblioteca
(provide tomar-prestamo)
(define (tomar-prestamo biblioteca id-usr id-libro dias-solicitados fecha-actual) ;RF18
  (let ((libro-disp? (libro-disponible? biblioteca id-libro))
        (dias-max-prestamo (obtener-max-dias-prestamo biblioteca))
        (limite-deuda (obtener-limite-deuda biblioteca))
        (libros-max (obtener-max-libros biblioteca))
        (libros-usr (prestamos-activos-usr biblioteca id-usr)))

    ;se asume que usuario ingresado esta registrado en la biblioteca
    (if (usuario-suspendido? (obtener-usuario biblioteca id-usr)) ;aca hay composicion
        biblioteca ; si usuario suspendido, se devuelve biblioteca si cambios
        (let ((deuda-usr (obtener-deuda (obtener-usuario biblioteca id-usr)))) ;composicion
          ; si no se rompe ninguna regla de la biblitoeca, se otorga el prestamo
          (if (and libro-disp?
                   (<= dias-solicitados dias-max-prestamo) 
                   (< deuda-usr limite-deuda) 
                   (< (length libros-usr) libros-max))

              ; se prepara id de prestamo y se acutaliza lista de prestamos 
              (let ((id-nuevo-prestamo (crear-id-prestamo biblioteca))
                    (lista-antigua-prestamos (obtener-prestamos biblioteca)))
                (let ((nuevo-prestamo (crear-prestamo id-nuevo-prestamo id-usr id-libro fecha-actual dias-solicitados)))
                  (let ((nueva-lista-prestamos (agregar-inicio-lista nuevo-prestamo lista-antigua-prestamos)))
                    ;se devuelve lista de prestamos acutalizada
                    (crear-biblioteca (libros-en-biblioteca biblioteca) 
                                      (usuarios-biblioteca biblioteca)
                                      nueva-lista-prestamos
                                      (obtener-max-libros biblioteca)
                                      (obtener-max-dias-prestamo biblioteca)
                                      (obtener-tasa-multa biblioteca)
                                      (obtener-limite-deuda biblioteca)
                                      (obtener-max-retraso-biblioteca biblioteca)
                                      (get-fecha biblioteca)))))
              biblioteca) ; fallo en alguna verificacion, se devuelve biblioteca sin cambios
          ))))

;se devuelve libro a biblioteca
;dominio: biblioteca, int, int, str
;recorrido: biblioteca
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
              (prest (car prestamo))); para poder utilizar el prestamo
          (let ((multa (calcular-multa prest fecha-actual (obtener-tasa-multa biblioteca)))
                (limite-deuda (obtener-limite-deuda biblioteca))
                (historial-usuario (historial-usr biblioteca id-usr)))
            (let ((nueva-deuda (obtener-deuda usuario)))
              (let ((suspender? (or (>= nueva-deuda limite-deuda) (hay-atraso? historial-usuario fecha-actual))))
                ;se actualiza usuario y prestamo
                (let ((usuario-actualizado (modificar-usuario-deuda-estado usuario nueva-deuda suspender?))
                      (prestamo-actualizado (modificar-estado-prestamo prest #f)))
                  (let ((nueva-lista-usuarios (map (lambda (u) ; se modifica lista de usr en la biblioteca
                                                     (if (= (id-usuario u) id-usr)
                                                         usuario-actualizado
                                                         u))
                                                   (usuarios-biblioteca biblioteca)))
                        
                        (nueva-lista-prestamos (map (lambda (p) ; se modifica lista de prestamos
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
                                      (obtener-max-retraso-biblioteca biblioteca)
                                      (get-fecha biblioteca))
                    ))))))))) 

;funcion que decide si usuario debe suspenderese por retraso excesivo o por superar limite de deuda
;dominio: biblioteca, int, str
;recorrido: bool
(provide debe-suspenderse?)
(define (debe-suspenderse? biblioteca id-usr fecha-actual);RF20
  ;funcion que agrupa en lista todos los atrasos excesivos del usuario
  (define (atraso-excesivo prestamos-act max-retraso)
    (filter (lambda (p)
              (> (calcular-dias-retraso fecha-actual (obtener-fecha-vencimiento p))
                 max-retraso)) prestamos-act)
    
    )
  (let ((historial-activo-usuario (prestamos-activos-usr biblioteca id-usr))
        (limite-maximo (obtener-limite-deuda biblioteca))
        (atraso-max (obtener-max-retraso-biblioteca biblioteca))
        (deuda-usr (obtener-deuda (obtener-usuario biblioteca id-usr))))
    ;si deuda es mayor al limite o hay algun libro con atraso excesivo, se retorna #t
    (if (or (> deuda-usr limite-maximo) (not (null? (atraso-excesivo historial-activo-usuario atraso-max))))
        #t
        #f
        )
    ))

;suspende manualmente al usuario en la biblioteca
;dominio: biblioteca, int
;recorrido: biblioteca
(provide suspender-usuario)
(define (suspender-usuario biblioteca id-usr) ;rf21
  (let ((usuario (obtener-usuario biblioteca id-usr)))
    ; si usuario esta suspendido se devuelve biblioteca sin cambio
    (if (usuario-suspendido? usuario)
        biblioteca; si usr ya suspendido, se devuelve bilbioteca intacta
        (let ((usuario-suspendido (suspender usuario)) ; se suspende manualmente al usuario
              (lista-usuarios (usuarios-biblioteca biblioteca)))
          ; se actualiza la lista de usuarios de la biblioteca
          (let ((nueva-lista-usuarios 
                 (map (lambda (usuario-actual) ; si coincide id de usuario, se cambia al usaurio suspendido
                        (if (= (id-usuario usuario-actual) id-usr)
                            usuario-suspendido
                            usuario-actual))
                      lista-usuarios)))
            ; se devuelve la biblioteca actualizada
            (crear-biblioteca (libros-en-biblioteca biblioteca)
                              nueva-lista-usuarios
                              (obtener-prestamos biblioteca)
                              (obtener-max-libros biblioteca)
                              (obtener-max-dias-prestamo biblioteca)
                              (obtener-tasa-multa biblioteca)
                              (obtener-limite-deuda biblioteca)
                              (obtener-max-retraso-biblioteca biblioteca)
                              (get-fecha biblioteca))
            )))))

;funcion currificada que permite renovar prestamo por n dias mas, siempre y cuando no suspere el maximo de dias de prestamo
;dominio: biblioteca, int, int, str
(provide renovar-prestamo);rf22
(define renovar-prestamo
  (lambda (biblioteca)
    (lambda(id-prest)
      (lambda (dias-extra)
        (lambda (fecha-actual)
          (let ((prestamo (obtener-prestamo biblioteca id-prest)))
            (if (null? prestamo)
                biblioteca ;si no se encuentra prestamo, se devulve biblioteca sin cambios
                (let ((estado-prestamo (get-estado-prestamo prestamo)) ; estado-prestamo: activo->#t, completado->#f
                      (usuario (obtener-usuario biblioteca (id-usuario-prestamo prestamo)))
                      (retraso (calcular-dias-retraso fecha-actual (obtener-fecha-vencimiento prestamo)));verifica que no hay retraso en el prestamo
                      ;verifica que no se exceda maximo de dias de prestamo prestamo 
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
                  ))))))))

;se paga deuda del usuario y se habilita si pago toda su deuda
(provide pagar-deuda)
(define (pagar-deuda biblioteca id-usr monto) ; rf23
  (let ((usuario (obtener-usuario biblioteca id-usr))
        (historial-activo-usuario (prestamos-activos-usr biblioteca id-usr))
        (fecha-actual (get-fecha biblioteca))
        (max-retraso (obtener-max-retraso-biblioteca biblioteca)))

    ;entrega lista con todos los atrasos excesivos de prestamos activos del usuario
    (define (atraso-excesivo prestamos-act max-retraso)
      (filter (lambda (p)
                (> (calcular-dias-retraso fecha-actual (obtener-fecha-vencimiento p))
                   max-retraso)) prestamos-act))
    (let ((deuda-usr (obtener-deuda usuario))
          (retraso? (if (null? (atraso-excesivo historial-activo-usuario max-retraso)) #f #t )))
      
      ;si paga toda su deuda, deduda=0 si no, deuda = diferencia entre deuda y lo pagado
      (let ((nueva-deuda (if (< (- deuda-usr monto) 0) 0 (- deuda-usr monto)))) 
        ;si paga deuda y no hay retraso estado del usuario para a ser #f (no suspendido)
        (let ((nuevo-estado-usr (not (and (= nueva-deuda 0) (not retraso?))))) 
          (let ((usr-actualizado (modificar-usuario-deuda-estado usuario nueva-deuda nuevo-estado-usr)))
            ;se modifica lista de usuarios
            (let ((nueva-lista-usuarios (map (lambda (u)
                                               (if (= (id-usuario u) id-usr)
                                                   usr-actualizado
                                                   u
                                                   )) (usuarios-biblioteca biblioteca))))
              ;se entrega nueva biblioteca
              (crear-biblioteca (libros-en-biblioteca biblioteca)
                                nueva-lista-usuarios
                                (obtener-prestamos biblioteca)
                                (obtener-max-libros biblioteca)
                                (obtener-max-dias-prestamo biblioteca)
                                (obtener-tasa-multa biblioteca)
                                (obtener-limite-deuda biblioteca)
                                (obtener-max-retraso-biblioteca biblioteca)
                                (get-fecha biblioteca))
              )))))))
;entrega hisotrial de prestamos de un usuario en formato str, si no hay usr o no tiene prestamos, se entrega str vacia
;dominio: biblioteca, int
;recorrido: str
(provide historial-prestamos-usuario)
(define (historial-prestamos-usuario biblioteca id-usr) ;RF24
  (let ((usuario (obtener-usuario biblioteca id-usr)))
    (if (null? usuario)
        "" ;string vacia  
        (let ((historial-usuario (historial-usr biblioteca id-usr))
              (primera-linea (string-append "Historial usuario " (number->string (id-usuario usuario)) ":\n")))
          (if (null? historial-usuario)
              "";string vacia
              (string-join (cons primera-linea (map print-prestamo historial-usuario)) "")) 
          )
        )))
;entrega historial de todos los prestamos de la biblioteca
;dominio: biblioteca
;recorrido: str
(provide historial-prestamos-sistema)
(define (historial-prestamos-sistema bibliotca)
  (define (print-prestamo-sistema prestamo)
    (let ((estado-str (if (get-estado-prestamo prestamo) "ACTIVO" "COMPLETADO")))
      (string-append
      
       "Préstamo #"
       (number->string (id-prestamo prestamo))
       ": Usuario "
       (number->string (id-usuario-prestamo prestamo))
       " - Libro "
       (number->string (id-libro-prestado prestamo))
       " - Fecha: "
       (fecha-prestamo prestamo)
       " - Días: "
       (number->string (duracion-prestamo prestamo))
       " "
       estado-str
       "\n")))
  
  (let ((prestamos-sistema (obtener-prestamos bibliotca)))
    (string-join (map print-prestamo-sistema prestamos-sistema)  "" ))
  )

;se suma un dia a la fecha actual de la bilioteca y se procesan multas
;dominio: biblioteca
;recorrido: bilbioteca
(provide procesar-dia);rf26
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
                        (obtener-max-retraso-biblioteca biblioteca)
                        nueva-fecha-sistema))))



(display "--- configurando datos de prueba... ---\n")
; libros de prueba
(define libro1 (crear-libro 1 "el hobbit" "j.r.r. tolkien"))
(define libro2 (crear-libro 2 "la comunidad del anillo" "j.r.r. tolkien"))
(define libro3 (crear-libro 3 "1984" "george orwell"))

; usuarios de prueba
(define usuario1 (crear-usuario 1 "jose"))
(define usuario2 (crear-usuario 2 "maria"))
(define usuario3-suspendido (modificar-usuario-deuda-estado (crear-usuario 3 "pedro") 0 #t))
(define usuario4-deudor (modificar-usuario-deuda-estado (crear-usuario 4 "ana") 1500 #f))

; prestamos de prueba
(define prestamo1 (crear-prestamo 101 1 1 "02/01" 3)) ; vence el 05/01
(define prestamo2 (crear-prestamo 102 2 2 "03/01" 5)) ; vence el 08/01
(define prestamo-atrasado (crear-prestamo 103 4 3 "10/01" 2)) ; vence el 12/01

; bibliotecas de prueba
(define b-vacia (crear-biblioteca '() '() '() 3 10 100 1000 5 "01/01"))

(define b-con-datos
  (crear-biblioteca
   (list libro1 libro2 libro3)
   (list usuario1 usuario2 usuario4-deudor)
   (list prestamo1 prestamo2 prestamo-atrasado)
   3 10 100 1000 5 "15/01"))


(libro-disponible? b-vacia 01)
(historial-prestamos-usuario b-con-datos 3)