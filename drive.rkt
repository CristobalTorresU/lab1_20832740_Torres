#lang racket
(require "funciones.rkt")
(provide add-drive)
(provide insertar-eleccion)

;Implementación TDA drive

;CONSTRUCTOR
;descripción: Permite añadir una unidad de almacenamiento al sistema
;dom: system x letter (char) x name (string) x capacity (int)
;rec: system
(define add-drive (lambda (system)
                    (lambda (letra name capacity)
                      (insertar (list-ref system 0)
                                (agregar-lista (list-ref system 1) (list letra name capacity))
                                (list-ref system 2)
                                (list-ref system 3)
                                (list-ref system 4)))))
