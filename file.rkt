#lang racket
(require "funciones.rkt")
(provide file)
(provide add-file)

;Implementación del TDA file

;CONSTRUCTOR
;descripción: Función que crea un archivo.
;dom: nombre (String) x tipo_archivo (String) x contenido (String)
;rec: file
(define file (lambda (nombre tipo_archivo contenido)
               (list nombre tipo_archivo contenido)))

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
                               (list-ref system 2)))))

;(resto_carpetas (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))

;
(define agregar_archivo_a_carpeta (lambda (carpeta file)
                                    (if (or (null? (archivos carpeta)) (equal? #t (buscar_archivos (nombres_archivos_unidad (archivos carpeta) null) (nombre_archivo file))))
                                        (append carpeta (list file))
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
                         (if (equal? nombre (formar_ruta (cdaar carpetas) "" (caaar carpetas)))
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