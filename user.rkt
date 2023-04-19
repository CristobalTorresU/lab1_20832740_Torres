#lang racket
(require "fecha.rkt")
(provide (all-defined-out))

;Implementación del TDA user

#|REPRESENTACIÓN: |#
;CONSTRUCTOR
;descripción: Función que permite crear un usuario (user).
;recursión: no
;dom: userName (String)
;rec: user
(define user (lambda (userName)
               (list userName (fecha))))

;SELECTORES

(define usuario_actual (lambda (system) (list-ref (car system) 3))) ;selecciona el usuario que esta realizando las operaciones

;MODIFICADORES

;descripción: Función que permite cambiar al usuario realizando las operaciones en el sistema.
;recursión: no
;dom: system x userName (String)
;rec: 
(define modificar_user (lambda (datos_sistema userName)
                         (list (list-ref datos_sistema 0)
                               (list-ref datos_sistema 1)
                               (list-ref datos_sistema 2)
                               userName
                               (list-ref datos_sistema 4))))

;descripción: Función que agrega un usuario a la lista de usuarios.
;recursión: no
;dom: usuarios (lista de users) x nuevo_usuario (user)
;rec: usuarios
(define agregar_usuario (lambda (usuarios nuevo_usuario)
                            (append usuarios (list nuevo_usuario))))

;PERTENENCIA

;descripción: Función que busca el nombre de un usuario
;recursión: sí, recursión natural, porque
;dom: userName (String) x usuarios
;rec: boolean
(define buscar_usuario (lambda (userName usuarios)
                         (if (null? usuarios) #f
                             (if (equal? userName (caar usuarios))
                                 #t
                                 (buscar_usuario userName (cdr usuarios))))))

;OTRAS OPERACIONES