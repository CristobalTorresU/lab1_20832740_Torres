#lang racket
(require "system.rkt" "fecha.rkt" "user.rkt")
(provide (all-defined-out))

;Implementación TDA drive

#|REPRESENTACIÓN: Este TDA representa un drive (unidad de almacenamiento), con sus datos y carpetas.
    Se utiliza una lista que contiene la letra del drive como char, el nombre y la capacidad como string,
y después las carpetas como TDA folder.|#

;CONSTRUCTORES
;descripción: Función que crea un drive.
;recursión: no
;dom: usuario_creador (String) x letter (Char) x name (String) x capacity (String)
;rec: drive
(define drive (lambda (creador letter name capacity)
                (list (char-downcase letter) name capacity (list (list (list (string (char-downcase letter) #\:)) creador (fecha) (fecha) null)))))

;SELECTORES

(define unidad_actual caadr) ;selecciona la unidad en la que se realizan las operaciones

(define resto_unidades cdadr) ;selecciona el resto de unidades en el que no se realizan las operaciones

(define letra_unidad car) ;selecciona la letra de la unidad en la que se realizan las operaciones

(define nombre_unidad cadr) ;selecciona el nombre de la unidad en la que se realizan las operaciones

(define size_unidad caddr) ;selecciona el tamaño de la unidad en la que se realizan las operaciones

;MODIFICADORES

;descripción: Función que permite modificar el path (ruta en la que se realizan las operaciones).
;recursión: no
;dom: system x path (list)
;rec: datos_sistema (actualizado)
(define modificar_path (lambda (datos_sistema path)
                         (list (list-ref datos_sistema 0)
                               (list-ref datos_sistema 1)
                               (list-ref datos_sistema 2)
                               (list-ref datos_sistema 3)
                               path)))

;descripción: Función que permite el drive actual (en el que se realizan las operaciones) del sistema.
;recursión: no
;dom: system x letter (char)
;rec: 
(define modificar_drive (lambda (datos_sistema letter)
                         (list (list-ref datos_sistema 0)
                               (list-ref datos_sistema 1)
                               letter
                               (list-ref datos_sistema 3)
                               (list-ref datos_sistema 4))))

;descripción: Función que agrega un drive al sistema.
;recursión: no
;dom: drives (lista de drives) x nuevo_drive (drive)
;rec: drives
(define agregar_drive (lambda (drives nuevo_drive)
                            (append drives (list nuevo_drive))))

;OTRAS OPERACIONES

;descripción: Función que reordena los drives (unidades) del sistema.
;recursión: sí, recursión natural, porque se avanza y reordenan los drives hasta que se cumpla la condición.
;dom: drives x letter (char)
;rec: drives
(define ordenar_drives (lambda (drives letter)
                         (if (equal? letter (caar drives))
                             drives
                             (ordenar_drives (append (cdr drives) (list (car drives))) letter))))

;descripión: Función que busca si existe un drive en el sistema.
;recursión: sí, recursión natural, porque recorre la lista de drives hasta encontrar el ingresado.
;dom: letter (char) x drives
;rec: boolean
(define buscar_drive (lambda (letter drives)
                         (if (null? drives) #f
                             (if (equal? letter (caar drives))
                                 #t
                                 (buscar_drive letter (cdr drives))))))