#lang racket
(provide insertar)
(provide agregar-lista)

;
;descripción: Función que inserta los elementos del sistema.
;dom: sys1 x sys2 x sys3 x sys4
;rec: system
(define insertar (lambda (sys1 sys2 sys3 sys4)
                   (list sys1 sys2 sys3 sys4)))

;
;
;
;
(define agregar-lista (lambda (objeto_1 objeto_2)
                            (append objeto_1 (list objeto_2))))
