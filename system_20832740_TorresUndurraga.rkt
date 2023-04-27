#lang racket
(provide (all-defined-out))

;Implementación del TDA system

#|REPRESENTACIÓN: Este TDA representa un system (sistema) y sus datos.
    Se representa con una lista de listas, siendo la primera los datos_sistema, que contiene
name como string, hora y fecha de creación como el TDA fecha, la letra del la unidad que se
está ocupando como char, el usuario actual como string y la ruta actual como una lista.
El segundo son las unidades del sistema como listas, el tercero los usuarios del sistema como listas,
y el cuarto como la papelera del sistema como listas.|#

;SELECTORES

(define datos_sistema (lambda (system) (list-ref system 0))) ;selecciona los datos del sistema.

(define usuario_actual (lambda (system) (list-ref (car system) 3))) ;selecciona el usuario que esta realizando las operaciones.

(define ruta_actual (lambda (system) (list-ref (car system) 4))) ;selecciona la ruta en la que se realizan las operaciones.

(define unidades (lambda (system) (list-ref system 1))) ;selecciona las unidades del sistema.

(define usuarios (lambda (system) (list-ref system 2))) ;selecciona los usuarios del sistema.

(define papelera (lambda (system) (list-ref system 3))) ;selecciona la papelera del sistema.

;MODIFICADORES

;descripción: Función que arma un sistema.
;recursión: no
;dom: datos_sistema x unidades x usuarios x papelera
;rec: system
(define armar_sistema (lambda (datos_sistema unidades usuarios papelera)
                        (list datos_sistema unidades usuarios papelera)))