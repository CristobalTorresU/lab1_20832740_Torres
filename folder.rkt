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
                   (if (equal? #t (comparar_rutas (rutas (cdddar (list-ref system 1)) null) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))
                       (insertar (list-ref system 0)
                             (append (list (append (car (list-ref system 1))
                                             (list (list (append (list-ref (car system) 4) (list name))))))
                                     (cdr (list-ref system 1)))
                             (list-ref system 2))
                       system))))

;OTRAS OPERACIONES
;descripción: 
;recursión: 
;dom: 
;rec: 
(define formar_ruta (lambda (path folder_name name)
                      (if (null? path)
                          (if (equal? "" folder_name)
                              (string-append name "/")
                              (string-append name "/" folder_name "/"))
                          (formar_ruta (cdr path) folder_name (string-append name "/" (car path))))))

;descripción: 
;dom: 
;rec: 
(define ruta_actual (lambda (system) (list-ref (car system) 4)))

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

;descripción: 
;dom: 
;rec: 
(define rutas (lambda (carpetas lista)
                (if (null? carpetas)
                    lista
                    (rutas (cdr carpetas) (append lista (list (formar_ruta (cdaar carpetas) "" (caaar carpetas))))))))

;descripción: 
;dom: 
;rec: 
(define comparar_rutas (lambda (rutas path)
                         (if (null? (filter (lambda (rutas) (equal? rutas path)) rutas))
                             #t
                             #f)))