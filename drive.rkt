#lang racket
;CONSTRUCTOR
;descripción: Permite añadir una unidad de almacenamiento al sistema
;dom: system x letter (char) x name (string) x capacity (int)
;rec: system
(define add-drive (lambda (system)
                    (lambda (letra name capacity)
                      (if (not (= 1 (length system)))
                          (if (>= 2 (length (car (cdr system))))
                              (append (list (car system)) (list (append (car (cdr system)) (list (list letra name capacity)))))
                              (append (list (car system))
                                      (list (append (cdr system) (list (list letra name capacity))))))
                     (append system  (list (list letra name capacity)))))))