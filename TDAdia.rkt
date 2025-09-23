#lang racket


;;-----------------------------definicion-----------------------------
; crea la representacion de una fecha
; dominio: int, int
; recorrido: fecha
(provide crear-fecha)
(define (crear-fecha dia mes) (list dia mes))

;;-----------------------------selectores-----------------------------
; obtiene el dia de una fecha
; dominio: fecha
; recorrido: int
(define (get-dia fecha) (list-ref fecha 0))

; obtiene el mes de una fecha
; dominio: fecha
; recorrido: int
(define (get-mes fecha) (list-ref fecha 1))

;;modificadores
; modififica el dia de una fecha
; dominio: fecha
; recorrido: fecha
(define (modificar-dia fecha dia)
  (crear-fecha dia (get-mes fecha)))

; modififica el mes de una fecha
; dominio: fecha
; recorrido: fecha
(define (modificar-mes fecha mes)
  (crear-fecha (get-dia fecha) mes))

;;-----------------------------pertenencia-----------------------------
; valida que el dato ingresado sea una fecha
; dominio: cualquier tipo de dato
; recorrido: bool
(define (fecha? fecha)
  (and (number? (get-dia fecha))
       (number? (get-mes fecha))
       (= (length fecha) 2)))

;---------------------------------otros-----------------------------

; lee fecha en formato dd/mm y entrega fecha
; dominio: string
; recorrido: fecha
(provide leer-fecha)
(define (leer-fecha str-fecha) 
  (let ((dia (substring str-fecha 0 2))
        (mes (substring str-fecha 3 5)))
    (crear-fecha (string->number dia) (string->number mes))))

;agrega un caracter cero en caso de que el numero sea menor a 10, para mantener formato
;de la fecha
;domino: int
;recorrido: str
(define (agregar-cero n) ;;aux
  (if (< n 10)
      (string-append "0" (number->string n))
      (number->string n))
  )

;convierte un tdafecha a string
;domino: fecha
;recorrido: string
(provide fecha->string)
(define (fecha->string fecha) 
  (let ((dia (get-dia fecha))
        (mes (get-mes fecha)))
    (string-append (agregar-cero dia) "/" (agregar-cero mes))
    )
  )
;obtiene la cantidad de dias en una fecha
;dominio: fecha
;recorrido: int
; dias empiezan en 0 por lo que primer mes va del 0-29, segundo mes 30-59 y asi 
(define (fecha->total-dias fecha ) ;aux
  (+ (* (- (get-mes fecha) 1) 30) (get-dia fecha)))

;convierte un numero a una fecha
;domino: int
;recorrido; fecha
(define (total-dias->fecha dias) ;aux 
  ; se sigue la misma convencion de dias indice 0
  (let ((dias-base-cero (- dias 1)))
    (let ((nuevo-mes (+ (modulo (quotient dias-base-cero 30)  12) 1))
          (nuevo-dia (+ (modulo dias-base-cero 30)1)))
      (crear-fecha nuevo-dia nuevo-mes))))

;funcion principal para suamr los dias, suma n dias a una fecha
;domino: fecha, int
;recorrido: fecha
(provide sumar-dias)
(define (sumar-dias fecha n)
  (total-dias->fecha (+ (fecha->total-dias fecha) n)))

;caclula la diferencia entre 2 fechas
;domino: fecha, fecha
;recorrido: int
(provide diferencia-dias)
(define (diferencia-dias fecha-inicio fecha-fin)
  (- (fecha->total-dias fecha-inicio) (fecha->total-dias fecha-fin)))

