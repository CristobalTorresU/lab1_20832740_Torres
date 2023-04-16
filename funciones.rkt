#lang racket
(provide insertar)
(provide agregar-lista)
(provide unidad_actual)
(provide resto_unidades)
(provide letra_unidad)
(provide nombre_unidad)
(provide size_unidad)
(provide carpetas)
(provide buscar_archivos)
(provide nombres_archivos_unidad)
(provide nombre_archivo)
(provide archivos)
(provide resto_carpetas)
(provide carpeta_actual)
(provide agregar_archivo_a_carpeta)
(provide seleccionar_archivo)
(provide run)
(provide usuario_actual)
(provide comparar_rutas)

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

;
(define unidad_actual caadr)

;
(define resto_unidades cdadr)

;
(define letra_unidad car)

;
(define nombre_unidad cadr)

;
(define size_unidad caddr)

;
(define carpetas cdddr)

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
(define agregar_archivo_a_carpeta (lambda (carpeta file)
                                    (if (or (null? (archivos carpeta)) (equal? #t (buscar_archivos (nombres_archivos_unidad (archivos carpeta) null) (nombre_archivo file))))
                                        (append carpeta (list file))
                                        carpeta)))

;
(define seleccionar_archivo (lambda (archivos name)
                                  (if (equal? name (caar archivos))
                                      (car archivos)
                                      (seleccionar_archivo (cdr archivos) name))))

;
(define run (lambda (system command) (command system)))

;
(define usuario_actual (lambda (system)
                         (list-ref (car system) 3)))

;
(define comparar_rutas (lambda (rutas path)
                         (if (null? (filter (lambda (rutas) (equal? rutas path)) rutas))
                             #t
                             #f)))
;
(define rutas (lambda (carpetas lista)
                (if (null? carpetas)
                    lista
                    (rutas (cdr carpetas) (append lista (list (formar_ruta (cdr (direccion_carpeta (car carpetas))) "" (car (direccion_carpeta (car carpetas))))))))))

;
(define formar_ruta (lambda (path folder_name name)
                      (if (null? path)
                          (if (equal? "" folder_name)
                              (string-append name "/")
                              (string-append name "/" folder_name "/"))
                          (formar_ruta (cdr path) folder_name (string-append name "/" (car path))))))
;
(define direccion_carpeta (lambda (carpeta)
                            (caar carpeta)))