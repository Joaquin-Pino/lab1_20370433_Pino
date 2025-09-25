#lang racket

(require "TDAdia_20370433_PinoBarrios.rkt")
(require "TDAprestamo_20370433_PinoBarrios.rkt")
(require "TDAlibro_20370433_PinoBarrios.rkt")
(require "TDAusuario_20370433_PinoBarrios.rkt")
(require "TDAbiblioteca_20370433_PinoBarrios.rkt")


(provide
 prestamos-activos-usr
 crear-biblioteca
 libros-en-biblioteca
 usuarios-biblioteca
 obtener-prestamos
 obtener-max-libros
 obtener-max-dias-prestamo
 obtener-tasa-multa
 obtener-limite-deuda
 obtener-max-retraso-biblioteca
 get-fecha
 usuario-presente?
 libro-en-biblioteca?
 obtener-usuario
 agregar-libro
 registrar-usuario
 agregar-inicio-lista
 historial-usr
 buscar-libro
 libro-disponible?
 calcular-dias-retraso
 calcular-multa
 tomar-prestamo
 devolver-libro
 debe-suspenderse?
 suspender-usuario
 renovar-prestamo
 pagar-deuda
 historial-prestamos-usuario
 historial-prestamos-sistema
 procesar-dia)

(provide
 crear-fecha
 leer-fecha
 fecha->string
 sumar-dias
 diferencia-dias)

(provide
 crear-libro
 get-libro-id
 get-libro-titulo
 get-libro-autor
 libro?)

(provide
 crear-prestamo
 id-prestamo
 id-usuario-prestamo
 id-libro-prestado
 fecha-prestamo
 duracion-prestamo
 get-estado-prestamo
 cambiar-duracion-prestamo
 modificar-estado-prestamo
 obtener-fecha-vencimiento)

(provide
 crear-usuario
 id-usuario
 obtener-deuda
 usuario-suspendido?
 suspender
 modificar-usuario-deuda-estado
 usuario?)