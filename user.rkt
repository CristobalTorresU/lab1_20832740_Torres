#lang racket
(require "funciones.rkt")
(provide register)
(provide login)
(provide logout)
(provide buscar_usuario)
(provide modificar_user)

;Implementación del TDA user

;CONSTRUCTOR
;descripción: Función que permite registrar un usuario al sistema.
;dom: system x userName
;rec: system
(define register (lambda (system)
                   (lambda (userName)
                     (insertar (list-ref system 0)
                               (list-ref system 1)
                               (if (equal? #t (buscar_usuario userName (list-ref system 2)))
                                   (list-ref system 2)
                                   (agregar-lista (list-ref system 2) (list userName)))))))

;SELECTOR
;descripción: Función que permite iniciar sesión con un usuario del sistema.
;dom: system x userName (String)
;rec: system
(define login (lambda (system)
                (lambda (userName)
                       (if (equal? #t (buscar_usuario userName (list-ref system 2)))
                           (insertar (modificar_user (car system) userName)
                                     (list-ref system 1)
                                     (list-ref system 2))
                           system))))

;SELECTOR
;descripión: Función que permite iniciar sesión.
;dom: system x userName (String)
;rec: system
(define logout (lambda (system)
                 (insertar (modificar_user (car system) "N/A")
                           (list-ref system 1)
                           (list-ref system 2))))

;PERTENENCIA
;descripción: Función que busca el nombre de un usuario
;dom: userName (String) x usuarios
;rec: boolean
(define buscar_usuario (lambda (userName usuarios)
                         (if (null? usuarios) #f
                             (if (equal? userName (caar usuarios))
                                 #t
                                 (buscar_usuario userName (cdr usuarios))))))

;funcion_modificar
(define modificar_user (lambda (system userName)
                         (list (list-ref system 0)
                               (list-ref system 1)
                               (list-ref system 2)
                               userName
                               (list-ref system 4))))
