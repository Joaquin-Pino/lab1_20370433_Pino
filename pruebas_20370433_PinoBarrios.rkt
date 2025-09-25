#lang racket

(require "main_20370433_PinoBarrios.rkt")


(display "===== SCRIPT DE PRUEBAS (CORREGIDO) =====\n\n")

; --------------- Configuracion inicial ---------------
(display "--- Configurando el sistema... ---\n")

; Crear libros y usuarios
(define l1 (crear-libro 101 "el principito" "antoine de saint-exupery"))
(define l2 (crear-libro 102 "juramentada" "brandon sanderson"))
(define l3 (crear-libro 103 "el imperio final" "brandon sanderson"))
(define u1 (crear-usuario 1 "ana"))
(define u2 (crear-usuario 2 "pedro"))

;;--------------- Crear la biblioteca con fecha 10/05---------------

(define b1 (crear-biblioteca (list l1 l2 l3) (list u1 u2) '() 2 2 100 500 2 "10/05"))

(display "Sistema listo. 3 libros, 2 usuarios, 100 de multa por dia de atraso,\n")
(display "500 deuda maxima, 2 dias de atraso maximo.\n\n")
(display "Fecha inicial: ") (display (get-fecha b1)) (newline)
(newline)

; --------------- inicio script ---------------

;prestamos iniciales
(display "=== DIA 10/05 ===\n")
(display "Ana toma prestado 'el principito' por 2 dias.\n")
(define b2 (tomar-prestamo b1 1 101 2 (get-fecha b1)))
(display "Ana toma prestado 'juramentada' por 2 dias.\n")
(define b3 (tomar-prestamo b2 1 102 2 (get-fecha b2)))
(display "Prestamos exitosos para ana.\n\n")

; procesamiento de tiempo
(display "=== Avanzando 3 dias en el tiempo... ===\n")
(define b4 (procesar-dia (procesar-dia (procesar-dia b3))))
(display "Fecha actual: ") (display (get-fecha b4)) (display " (13/05)\n")
(display "Los prestamos de ana ahora tienen 1 dia de retraso.\n")
(define ana-deudora (obtener-usuario b4 1))
(display "Deuda actual de ana: ") (display (obtener-deuda ana-deudora)) (display " (esperado 200)\n\n")

;devolucion de un libro
(display "=== DIA 13/05 - devolucion ===\n")
(display "Ana devuelve 'el principito'.\n")
(define b5 (devolver-libro b4 1 101 (get-fecha b4)))
(define ana-final (obtener-usuario b5 1))
(display "Deuda final de ana: ") (display (obtener-deuda ana-final))
; no hay deuda excesiva ni retraso excesivo, por lo que no se debe suspender
(display "Estado de suspension de ana: ") (display (usuario-suspendido? ana-final)) (display " (esperado #f, no suspendida)\n\n")

; usuario con dedua puede pedir
(display "=== DIA 13/05 - ana pide otro libro ===\n")
(display "Ana, al no estar suspendida, intenta tomar 'el imperio final'.\n")
(define b6 (tomar-prestamo b5 1 103 2 (get-fecha b5)))
(display "Prestamo exitoso?: ") (display (not (eq? b5 b6))) (display " (esperado #t, la biblioteca cambio)\n\n")

; usuario toma libro que antes estaba ocupado
(display "=== DIA 13/05 - pedro pide un libro ===\n")
(display "Pedro intenta tomar 'el principito', que ahora esta disponible.\n")
(define b7 (tomar-prestamo b6 2 101 2 (get-fecha b6)))
(display "Libro prestado a pedro con exito.\n\n")

; pago toal de la deuda, usuario deberia volver a estar activo
(display "=== DIA 13/05 - ana paga su deuda ===\n")
(display "Ana paga su deuda pendiente.\n")

(define b8 (pagar-deuda b7 1 300)) ; ana paga mas de lo que debe, pero cancela toda la deuda
(define ana-reactivada (obtener-usuario b8 1))
(display "Deuda restante de ana: $") (display (obtener-deuda ana-reactivada)) (newline)
(display "Estado de suspension de ana: ") (display (usuario-suspendido? ana-reactivada)) (display " (sigue #f = activa)\n\n")

; historial de usuario y de biblioteca
(display "===  Historial final del ana ===\n")
(display (historial-prestamos-usuario b8 1)) (newline)

(display "=== Estado final del sistema ===\n")
(display "Historial de todos los prestamos:\n")
(display (historial-prestamos-sistema b8))
(newline)

(display "===== FIN DEL SCRIPT =====\n")