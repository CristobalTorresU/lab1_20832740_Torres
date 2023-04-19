#lang racket
(require "system.rkt" "fecha.rkt" "user.rkt")
(provide (all-defined-out))

;Implementación TDA drive

#|REPRESENTACIÓN: |#

;CONSTRUCTORES
(define drive (lambda (system letter name capacity)
                (list (char-downcase letter) name capacity (list (list (list (string (char-downcase letter) #\:)) (usuario_actual system) (fecha) (fecha) null)))))

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
;rec: 
(define modificar_path (lambda (system path)
                         (list (datos_sistema system)
                               (unidades system)
                               (usuarios system)
                               (papelera system)
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

;PERTENENCIA

;descripión: Función que busca si existe un drive en el sistema.
;recursión: sí, recursión natural, porque recorre la lista de drives.
;dom: letter (char) x drives
;rec: boolean
(define buscar_drive (lambda (letter drives)
                         (if (null? drives) #f
                             (if (equal? letter (caar drives))
                                 #t
                                 (buscar_drive letter (cdr drives))))))

;OTRAS OPERACIONES

;descripción: Función que reordena los drives (unidades) del sistema.
;recursión: sí, recursión natural, porque
;dom: drives x letter (char)
;rec: drives
(define ordenar_drives (lambda (drives letter)
                         (if (equal? letter (caar drives))
                             drives
                             (ordenar_drives (append (cdr drives) (list (car drives))) letter))))