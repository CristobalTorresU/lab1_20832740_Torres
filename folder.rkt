#lang racket
(require "funciones.rkt")
(provide md)
(provide cd)

;Implementación del TDA folder

;CONSTRUCTOR
;descripión: Función que crea un directorio dentro de la unidad con un nombre especificado.
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

;SELECTOR
;descripción: Función que permite cambiar la ruta (path) en que se realizan las operaciones.
;dom: system x path or folderName (String)
;rec: system
(define cd (lambda (system)
             (lambda (path)
               (if (equal? path "/")
                   (volver_a_root system)
                   (if (equal? path "..")
                       (retroceder_carpeta system)
                       (if (equal? #f (comparar_rutas (rutas (cdddar (list-ref system 1)) null) (formar_ruta (cdr (ruta_actual system)) path (car (ruta_actual system)))))
                           (insertar (modificar_path (car system) (append (ruta_actual system) (list path)))
                                     (list-ref system 1)
                                     (list-ref system 2))
                   system))))))

;OTRAS OPERACIONES
;descripción: Función que forma la ruta que se ingresa.
;recursión: Recursión Natural
;dom: path (list) x folder_name (String) x name (String)
;rec: name (String)
(define formar_ruta (lambda (path folder_name name)
                      (if (null? path)
                          (if (equal? "" folder_name)
                              (string-append name "/")
                              (string-append name "/" folder_name "/"))
                          (formar_ruta (cdr path) folder_name (string-append name "/" (car path))))))

;SELECTOR
;descripción: Función que entrega la ruta en la que se encuentra el sistema.
;dom: system
;rec: ruta (list)
(define ruta_actual (lambda (system) (list-ref (car system) 4)))

;MODIFICADOR
;descripción: Función que cambia la ruta actual.
;dom: system x path (list)
;rec: system
(define modificar_path (lambda (system path)
                         (list (list-ref system 0)
                               (list-ref system 1)
                               (list-ref system 2)
                               (list-ref system 3)
                               path)))

;
;descripción: Función que muestra todas las rutas de la unidad.
;dom: carpetas (list) x lista (list)
;rec: lista (list)
(define rutas (lambda (carpetas lista)
                (if (null? carpetas)
                    lista
                    (rutas (cdr carpetas) (append lista (list (formar_ruta (cdaar carpetas) "" (caaar carpetas))))))))

;
;descripción: Función que define si una carpeta (ruta) ya existe.
;dom: rutas (list) x path (String)
;rec: boolean
(define comparar_rutas (lambda (rutas path)
                         (if (null? (filter (lambda (rutas) (equal? rutas path)) rutas))
                             #t
                             #f)))

;MODIFICADOR
;descripción: Función que retrocede a la carpeta anterior del directorio actual.
;dom: system
;rec: system
(define retroceder_carpeta (lambda (system)
                             (insertar (modificar_path (car system) (remover_carpeta_final (ruta_actual system)))
                                       (list-ref system 1)
                                       (list-ref system 2))))

;MODIFICADOR
;descripción: Función que quita la última carpeta del directorio actual.
;dom: path (list)
;rec: path (list)
(define remover_carpeta_final (lambda (path)
                                (if (= 1 (length path))
                                    path
                                    (reverse (cdr (reverse path))))))

;MODIFICADOR
;descripción: Función que cambia el directorio actual a root.
;dom: system
;rec: system
(define volver_a_root (lambda (system)
                        (insertar (modificar_path (car system) (root system))
                                  (list-ref system 1)
                                  (list-ref system 2))))

;SELECTOR
;descripción: Función que entrega la carpeta root de la unidad actual.
;dom: system
;rec: root (list)
(define root (lambda (system)
               (list (car (list-ref (car system) 4)))))