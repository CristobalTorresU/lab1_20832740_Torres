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

(define letra_unidad_actual (lambda (system) (list-ref (car system) 2))) ;selecciona la letra de la unidad en la que se realizan las operaciones.

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

;OTRAS OPERACIONES

;descripción: Función que forma el string que contiene el contenido de un directorio.
;recursión: sí, recursión natural, porque agrega cada elemento de las listas al string final.
;dom: string (String) x nombres_carpetas (lista de strings) x archivos (lista de strings)
;rec: string
(define formar_string (lambda (string nombres_carpetas nombres_archivos)
                        (if (null? nombres_carpetas)
                            (if (null? nombres_archivos)
                                string
                                (formar_string (string-append string "\n" (car nombres_archivos)) nombres_carpetas (cdr nombres_archivos)))
                            (formar_string (string-append string "\n" (car nombres_carpetas)) (cdr nombres_carpetas) nombres_archivos))))

;descripción: Función que busca y entrega uno de los strings buscados.
;recursión: sí, recursión natural, porque busca en cada elemento de la lista hasta encontrar uno de los strings.
;dom: args
;rec: string
(define encontrar_string_n (lambda (args)
                             (if (or (equal? "/o N" (car args)) (equal? "/o -N" (car args)))
                             (car args)
                             (encontrar_string_n (cdr args)))))

;descripción: Función que ordena alfabeticamente el string de la función dir de forma ascendete o descendente.
;recursión: no 
;dom: archivos x lista
;rec: lista
(define ordenar_alfabeticamente (lambda (opcion lista)
                                  (if (equal? #\- (string-ref opcion 3))
                                      (sort lista string>?)
                                      (sort lista string<?))))