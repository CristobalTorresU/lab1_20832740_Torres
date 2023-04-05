#lang racket
(provide insertar)
(provide agregar-lista)

;
;
;
;
(define insertar (lambda (sys1 sys2 sys3)
                   (list sys1 sys2 sys3)))

;
;
;
;
(define agregar-lista (lambda (objeto_1 objeto_2)
                            (append objeto_1 (list objeto_2))))