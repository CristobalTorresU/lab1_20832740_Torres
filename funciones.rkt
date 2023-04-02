#lang racket
(provide insertar)
(provide agregar-lista)
(provide insertar-eleccion)

(define insertar (lambda (sys1 sys2 sys3 sys4 sys5)
                   (list sys1 sys2 sys3 sys4 sys5)))

(define agregar-lista (lambda (objeto_1 objeto_2)
                        (if (not (null? objeto_1))
                        (if (= 1 (length (car objeto_1)))
                            (append objeto_1 (list objeto_2))
                            (append objeto_1 (list objeto_2)))
                        (list objeto_2))))

(define insertar-eleccion (lambda (eleccion lista)
                            (append (list eleccion) lista)))