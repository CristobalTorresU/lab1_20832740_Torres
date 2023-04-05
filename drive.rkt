#lang racket
(require "funciones.rkt")
(provide add-drive)
(provide buscar_drive)
(provide switch-drive)

;Implementación TDA drive

;CONSTRUCTOR
;descripción: Permite añadir una unidad de almacenamiento al sistema
;dom: system x letter (char) x name (string) x capacity (int)
;rec: system
(define add-drive (lambda (system)
                    (lambda (letter name capacity)
                      (if (equal? #f (buscar_drive letter (list-ref system 1)))
                      (insertar (list-ref system 0)
                                (agregar-lista (list-ref system 1) (list letter name capacity (list (list (string letter #\:)))))
                                (list-ref system 2))
                      system))))

;PERTENENCIA
;descripión: Función que busca si existe un drive
;dom: letter (char) x drives
;rec: boolean
(define buscar_drive (lambda (letter drives)
                         (if (null? drives) #f
                             (if (equal? letter (caar drives))
                                 #t
                                 (buscar_drive letter (cdr drives))))))

;SELECTOR
;descripión: Función que fija una unidad de almacenamiento.
;dom: system x letter (char)
;rec: system
(define switch-drive (lambda (system)
                       (lambda (letter)
                         (if (not (equal? "N/A" (list-ref (car system) 3)))
                             (if (equal? #t (buscar_drive letter (list-ref system 1)))
                                 (insertar (modificar_path (modificar_drive (car system) letter) (list (string letter #\:)))
                                           (ordenar_drives (list-ref system 1) letter)
                                           (list-ref system 2))
                                 system)
                             system))))

;
;
;
;
(define modificar_path (lambda (system path)
                         (list (list-ref system 0)
                               (list-ref system 1)
                               (list-ref system 2)
                               (list-ref system 3)
                               path)))

;
;descripción: 
;recursión: 
;dom: 
;rec: 
(define ordenar_drives (lambda (drives letter)
                         (if (equal? letter (caar drives))
                             drives
                             (ordenar_drives (append (cdr drives) (list (car drives))) letter))))

;
;
;
;
(define modificar_drive (lambda (system letter)
                         (list (list-ref system 0)
                               (list-ref system 1)
                               letter
                               (list-ref system 3)
                               (list-ref system 4))))