#lang racket
(require "funciones.rkt")
(provide register)
(provide login)
(provide logout)
(provide buscar_usuario)

;Implementación del TDA user

;CONSTRUCTOR
;descripción: Función que permite registrar un usuario al sistema.
;dom: system x userName
;rec: system
(define register (lambda (system)
                   (lambda (userName)
                     (insertar (list-ref system 0)
                               (list-ref system 1)
                               (if (equal? #t (buscar_usuario userName (cdr (list-ref system 2))))
                                   (list-ref system 2)
                                   (agregar-lista (list-ref system 2) (list userName)))
                               (list-ref system 3) (list-ref system 4)))))

;SELECTOR
;descripción: Función que permite iniciar sesión con un usuario del sistema.
;dom: system x userName (String)
;rec: system
(define login (lambda (system)
                (lambda (userName)
                  (insertar (list-ref system 0)
                            (list-ref system 1)
                            (if (equal? #t (buscar_usuario userName (cdr (list-ref system 2))))
                                (insertar-eleccion userName (cdr (list-ref system 2)))
                                (list-ref system 2))
                            (list-ref system 3)
                            (list-ref system 4)))))

;SELECTOR
;descripión: Función que permite iniciar sesión.
;dom: system x userName (String)
;rec: system
(define logout (lambda (system)
                 (insertar (list-ref system 0)
                           (list-ref system 1)
                           (append (list "N/A") (cdr (list-ref system 2)))
                           (list-ref system 3)
                           (list-ref system 4))))

;OTRAS OPERACIONES
;descripción: Función que busca el nombre de un usuario
;dom: userName (String) x usuarios
;rec: boolean
(define buscar_usuario (lambda (userName usuarios)
                         (if (null? usuarios) #f
                             (if (equal? userName (caar usuarios))
                                 #t
                                 (buscar_usuario userName (cdr usuarios))))))