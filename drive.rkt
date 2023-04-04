#lang racket
(require "funciones.rkt")
(provide add-drive)
(provide buscar_drive)
(provide elegir_drive)
(provide switch-drive)

;Implementación TDA drive

;CONSTRUCTOR
;descripción: Permite añadir una unidad de almacenamiento al sistema
;dom: system x letter (char) x name (string) x capacity (int)
;rec: system
(define add-drive (lambda (system)
                    (lambda (letter name capacity)
                      (if (equal? #f (buscar_drive letter (cdr (list-ref system 1))))
                      (insertar (list-ref system 0)
                                (agregar-lista (list-ref system 1) (list letter name capacity))
                                (list-ref system 2)
                                (agregar-lista (list-ref system 3) (list letter))
                                (list-ref system 4))
                      system))))

;SELECTOR
;descripión: Función que busca si existe un drive
;dom: letter (char) x drives
;rec: boolean
(define buscar_drive (lambda (letter drives)
                         (if (null? drives) #f
                             (if (equal? letter (caar drives))
                                 #t
                                 (buscar_drive letter (cdr drives))))))

;SELECTOR
;descripión: Función que elige el drive 
;dom: letter (char) x drives
;rec: drive
(define elegir_drive (lambda (letter drives)
                         (if (null? drives) #f
                             (if (equal? letter (caar drives))
                                 (car drives)
                                 (buscar_drive letter (cdr drives))))))

;SELECTOR
;descripión: Función que fija una unidad de almacenamiento.
;dom: system x letter (char)
;rec: system
(define switch-drive (lambda (system)
                       (lambda (letter)
                         (if (not (equal? "N/A" (car (list-ref system 2))))
                             (if (equal? #t (buscar_drive letter (cdr (list-ref system 1))))
                                 (insertar (list-ref system 0)
                                           (append (list (elegir_drive letter (cdr (list-ref system 1)))) (cdr (list-ref system 1)))
                                           (list-ref system 2)
                                           (list-ref system 3)
                                           (list-ref system 4))
                                 system)
                             system))))