#lang racket
(require "funciones.rkt" "fecha.rkt" "drive.rkt" "file.rkt")
(provide md)
(provide cd)
(provide rd)
(provide copy)
(provide carpeta)

;Implementación del TDA folder

;CONSTRUCTOR
;descripción: Función que crea una carpeta.
;dom: dirección (nombre) x fecha_creación (fecha) x fecha_modificación (fecha)
;rec: carpeta
(define carpeta (lambda (direccion creador . seguridad)
                  (list (list direccion creador (fecha) (fecha) seguridad))))

;CONSTRUCTOR
;descripión: Función que crea un directorio dentro de la unidad con un nombre especificado.
;dom: system x name (String)
;rec: system
(define md (lambda (system)
             (lambda (name)
                   (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))
                       (insertar (list-ref system 0)
                                 (append (list (append (car (list-ref system 1))
                                             (list (carpeta (append (list-ref (car system) 4) (list name)) (list-ref (car system) 3)))))
                                     (cdr (list-ref system 1)))
                             (list-ref system 2)
                             (list-ref system 3))
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
                       (if (equal? #f (comparar_rutas (rutas (carpetas_sistema (list-ref system 1) null) null) (string-downcase path)))
                           (insertar (modificar_path (car ((run system switch-drive) (string-ref (string-downcase path) 0))) (separar_string_ruta (string-downcase path)))
                                     (list-ref ((run system switch-drive) (string-ref (string-downcase path) 0)) 1)
                                     (list-ref system 2)
                                     (list-ref system 3))
                           (if (equal? #f (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) path (car (ruta_actual system)))))
                               (insertar (modificar_path (car system) (append (ruta_actual system) (separar_string_ruta path)))
                                         (list-ref system 1)
                                         (list-ref system 2)
                                         (list-ref system 3))
                   system)))))))

;SELECTOR
;descripción: Función que copia un archivo o carpeta desde una ruta origen a una ruta destino.
;dom: system x source (file or folder) (String) x target path (String)
;rec: system
(define copy (lambda (system)
               (lambda (source target)
                 (if (not (= 1 (length (separar_string source))))
                     (if (equal? #t (buscar_archivos (nombres_archivos_unidad (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) null) source))
                     system
                     (if (equal? #t (comparar_rutas (rutas (cdddar (list-ref system 1)) null) target))
                         system
                        (if (equal? #f (buscar_archivos (nombres_archivos_unidad (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) target))) null) source))
                             system
                             (insertar (list-ref system 0)
                                       (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                               (list (agregar_archivo_a_carpeta (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) target)) (seleccionar_archivo (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) source)))
                                               (resto_carpetas (primero_carpeta_actual (carpetas (unidad_actual system)) target))))
                                               (resto_unidades system))
                                       (list-ref system 2)
                                       (list-ref system 3)))))
                     (if (equal? #t (comparar_rutas (rutas (cdddar (list-ref system 1)) null) (formar_ruta (cdr (ruta_actual system)) source (car (ruta_actual system)))))
                         system
                         (if (equal? #t (comparar_rutas (rutas (cdddar (list-ref system 1)) null) target))
                             system
                             (if (equal? #f (comparar_rutas (rutas (cdddar (list-ref system 1)) null) (formar_ruta (cdr (separar_string_ruta target)) source (car (separar_string_ruta target)))))
                                 system
                                 (insertar (list-ref system 0)
                                 (append (list (append (car (list-ref system 1))
                                             (list (list (append (separar_string_ruta target) (list source))))))
                                     (cdr (list-ref system 1)))
                             (list-ref system 2)
                             (list-ref system 3)))))))))

;SELECTOR
;descripción: Función que mueve un archivo o carpeta desde una ruta origen a una ruta destino.
;dom: system x source (file or folder) (String) x target path (String)
;rec: system
(define move (lambda (system)
               (lambda (source target)
                 (if (not (= 1 (length (separar_string source))))
                     ((run ((run system copy) source target) del) source)
                     0))))

;descripción: Función que separa los strings en los ".".
;dom: name (String)
;rec: list
(define separar_string (lambda (name)
                         (string-split name "." #:trim? #t)))

;
(define separar_string_ruta (lambda (name)
                         (string-split name "/" #:trim? #t)))

;MODIFICADOR
;descripción: Función que elimina una carpeta que este vacía.
;dom: system x folderName o folderPath (String)
;rec: system
(define rd (lambda (system)
             (lambda (name)
               (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))
                   system
                   (if (and (equal? #f (tiene_archivos (car (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system))))))) (equal? #f (tiene_carpetas (carpetas_unidad_actual system) (append (ruta_actual system) (list name)) null)))
                       (insertar (list-ref system 0)
                                 (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                                     (cdr (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))))
                                       (resto_unidades system))
                                 (list-ref system 2)
                                 (list-ref system 3))
                       system)))))

;
(define tiene_carpetas (lambda (carpetas path lista)
                         (if (null? (filter (lambda (x) (igual_fuente x path)) (comparar carpetas path null)))
                             #f
                             #t)))

;
(define igual_fuente (lambda (path_carpeta path)
                       (if (null? path)
                           #t
                           (if (equal? (car path_carpeta) (car path))
                               (igual_fuente (cdr path_carpeta) (cdr path))
                               #f))))

;
(define comparar (lambda (carpetas path lista)
                   (if (null? carpetas)
                       lista
                       (if (> (length (caar carpetas)) (length path))
                           (comparar (cdr carpetas) path (append lista (car carpetas)))
                           (comparar (cdr carpetas) path lista)))))

;
(define tiene_archivos (lambda (carpeta)
                         (if (null? (cdr carpeta))
                             #f
                             #t)))

;
(define carpetas_unidad_actual (lambda (system)
                                 (cdddar (list-ref system 1))))

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
                    (rutas (cdr carpetas) (append lista (list (formar_ruta (cdr (direccion_carpeta (car carpetas))) "" (car (direccion_carpeta (car carpetas))))))))))

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
                                       (list-ref system 2)
                                       (list-ref system 3))))

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
                                  (list-ref system 2)
                                  (list-ref system 3))))

;SELECTOR
;descripción: Función que entrega la carpeta root de la unidad actual.
;dom: system
;rec: root (list)
(define root (lambda (system)
               (list (car (list-ref (car system) 4)))))

;OTRAS OPERACIONES
;descripción:
;dom:
;rec:
(define primero_carpeta_actual (lambda (carpetas nombre)
                         (if (equal? nombre (formar_ruta (cdr (direccion_carpeta (car carpetas))) "" (car (direccion_carpeta (car carpetas)))))
                             carpetas
                             (primero_carpeta_actual (append (cdr carpetas) (list (car carpetas))) nombre))))

;
(define direccion_carpeta (lambda (carpeta)
                            (caar carpeta)))

;
(define carpetas_sistema (lambda (unidades lista)
                           (if (null? unidades)
                               lista
                               (carpetas_sistema (cdr unidades) (append lista (cdddar unidades))))))
