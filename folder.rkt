#lang racket
(require "funciones.rkt")
(provide md)

;Implementación del TDA folder

;CONSTRUCTOR
;descripión:
;dom: system x name (String)
;rec: system
(define md (lambda (system)
             (lambda (name)
                   (insertar (list-ref system 0)
                             (append (list (append (car (list-ref system 1))
                                             (list (list (append (list-ref (car system) 4) (list name))))))
                                     (cdr (list-ref system 1)))
                             (list-ref system 2)))))

;OTRAS OPERACIONES
;descripción: 
;recursión: 
;dom: 
;rec: 
(define formar_ruta (lambda (path name)
                      (if (null? (cdr path))
                          (string-append (car path) "/" name "/")
                          (formar_ruta (list (string-append (car path) (cadr path))
                                             (if (null? (cddr path)) null (cddr path)))
                                       name))))

;MODIFICADOR
;descripción: 
;dom:
;rec:
(define modificar_path (lambda (system path)
                         (list (list-ref system 0)
                               (list-ref system 1)
                               (list-ref system 2)
                               (list-ref system 3)
                               path)))
