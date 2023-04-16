#lang racket
(require "funciones.rkt" "fecha.rkt")
(provide file)
(provide add-file)
(provide del)

;Implementación del TDA file

;CONSTRUCTOR
;descripción: Función que crea un archivo.
;dom: nombre (String) x tipo_archivo (String) x contenido (String)
;rec: file
(define file (lambda (nombre tipo_archivo contenido . seguridad)
               (list nombre tipo_archivo contenido seguridad)))

;MODIFICADOR
;descripción: Función que añade un archivo a la ruta actual.
;dom: system x file
;rec: system
(define add-file (lambda (system)
                   (lambda (file)
                     (insertar (list-ref system 0)
                               (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                               (list (agregar_archivo_a_carpeta (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) file))
                                               (resto_carpetas (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
                                       (resto_unidades system))
                               (list-ref system 2)
                               (list-ref system 3)))))

;MODIFICADOR
;descripción: Función que elimina uno o varios archivos del directorio actual.
;dom: system x fileName o fileNamePattern (String)
;rec: system
(define del (lambda (system)
              (lambda (name)
                (if (not (= 1 (length (separar_string name))))
                  (if (equal? #f (buscar_archivos (nombres_archivos_unidad (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) null) name))
                              (insertar (list-ref system 0)
                                        (eliminar_archivo system name)
                                        (list-ref system 2)
                                        (append (list-ref system 3) (list (seleccionar_archivo (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) name))))
                              system)
                  (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))
                      system
                      (insertar (list-ref system 0)
                                (append (list (append (list (caar (list-ref system 1)))
                                        (list (cadar (list-ref system 1)))
                                        (list (caddar (list-ref system 1)))
                                        (eliminar_carpetas (carpetas_unidad_actual system) (append (list (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))) (tiene_carpetas2 (carpetas_unidad_actual system) (append (ruta_actual system) (list name)))))))
                                        (cdr (list-ref system 1)))
                                (list-ref system 2)
                                (list-ref system 3)))))))

;
(define eliminar_carpetas (lambda (carpetas carpetas_a_eliminar)
                            (if (null? carpetas_a_eliminar)
                                carpetas
                                (eliminar_carpetas (remove (car carpetas_a_eliminar) carpetas) (cdr carpetas_a_eliminar)))))

;MODIFICADOR
;descripción: Función que elimina un archivo en particular.
;dom: system x name (String)
;rec: archivos (List)
(define eliminar_archivo (lambda (system name)
                           (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                               (list (remove (seleccionar_archivo (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) name)
                                                             (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
                                               (resto_carpetas (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
                                       (resto_unidades system))))

;SELECTOR
;descripción: Función que selecciona un archivo dentro de un directorio.
;dom: archivos (List) x name (String)
;rec: file
(define seleccionar_archivo (lambda (archivos name)
                                  (if (equal? name (caar archivos))
                                      (car archivos)
                                      (seleccionar_archivo (cdr archivos) name))))

;
(define agregar_archivo_a_carpeta (lambda (carpeta file)
                                    (if (or (null? (archivos carpeta)) (equal? #t (buscar_archivos (nombres_archivos_unidad (archivos carpeta) null) (nombre_archivo file))))
                                        (append (actualizar_fecha_modificacion carpeta) (cdr carpeta) (list file))
                                        carpeta)))

;
(define buscar_archivos (lambda (archivos file_name)
                         (if (null? (filter (lambda (archivos) (equal? archivos file_name)) archivos))
                             #t
                             #f)))

;
(define nombres_archivos_unidad (lambda (archivos lista)
                                  (if (null? archivos)
                                      lista
                                      (nombres_archivos_unidad (cdr archivos)
                                                               (append lista (list (nombre_archivo (car archivos))))))))

;
(define nombre_archivo car)

;
(define archivos cdr)

;
(define resto_carpetas cdr)

;
(define carpeta_actual car)

;
(define letra_unidad car)

;
(define nombre_unidad cadr)

;
(define size_unidad caddr)

;
(define unidad_actual caadr)

;
(define primero_carpeta_actual (lambda (carpetas nombre)
                         (if (equal? nombre (formar_ruta (cdr (direccion_carpeta (car carpetas))) "" (car (direccion_carpeta (car carpetas)))))
                             carpetas
                             (primero_carpeta_actual (append (cdr carpetas) (list (car carpetas))) nombre))))

;
(define carpetas cdddr)

;
(define formar_ruta (lambda (path folder_name name)
                      (if (null? path)
                          (if (equal? "" folder_name)
                              (string-append name "/")
                              (string-append name "/" folder_name "/"))
                          (formar_ruta (cdr path) folder_name (string-append name "/" (car path))))))

;
(define ruta_actual (lambda (system) (list-ref (car system) 4)))

;
(define resto_unidades cdadr)

;
(define direccion_carpeta (lambda (carpeta)
                            (caar carpeta)))

;
;
(define actualizar_fecha_modificacion (lambda (carpeta)
                                        (list (list (list-ref (car carpeta) 0)
                                                    (list-ref (car carpeta) 1)
                                                    (list-ref (car carpeta) 2)
                                                    (fecha)
                                                    (list-ref (car carpeta) 4)))))

;
(define separar_string (lambda (name)
                         (string-split name "." #:trim? #t)))

;
(define rutas (lambda (carpetas lista)
                (if (null? carpetas)
                    lista
                    (rutas (cdr carpetas) (append lista (list (formar_ruta (cdr (direccion_carpeta (car carpetas))) "" (car (direccion_carpeta (car carpetas))))))))))

;
(define carpetas_unidad_actual (lambda (system)
                                 (cdddar (list-ref system 1))))

;
(define tiene_carpetas2 (lambda (carpetas path)
                         (filter (lambda (x) (igual_fuente2 x path)) (comparar2 carpetas path null))))

;
;
(define igual_fuente2 (lambda (path_carpeta path)
                       (if (null? path)
                           #t
                           (if (equal? (caaar path_carpeta) (car path))
                               (igual_fuente2 (avanzar_directorio_carpeta path_carpeta) (cdr path))
                               #f))))

;
(define comparar2 (lambda (carpetas path lista)
                   (if (null? carpetas)
                       lista
                       (if (> (length (caaar carpetas)) (length path))
                           (comparar2 (cdr carpetas) path (append lista (list (car carpetas))))
                           (comparar2 (cdr carpetas) path lista)))))

;
(define avanzar_directorio_carpeta (lambda (carpeta)
                                     (append (list (append (list (cdaar carpeta)) (cdar carpeta))) (cdr carpeta))))
