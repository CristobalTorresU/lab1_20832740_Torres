#lang racket
(require "system_20832740_TorresUndurraga.rkt")
(require "drive_20832740_TorresUndurraga.rkt")
(require "user_20832740_TorresUndurraga.rkt")
(require "folder_20832740_TorresUndurraga.rkt")
(require "file_20832740_TorresUndurraga.rkt")
(require "fecha_20832740_TorresUndurraga.rkt")
(provide (all-defined-out))

;descripción: Permite crear un sistema y registra la fecha de creación.
;recursión: no
;dom: name (string)
;rec: system
(define system (lambda (name) (list (list name (fecha) "N/A" "N/A" "N/A") null null null)))

;descripción: Función que permite ejecutar un comando sobre un sistema.
;recursión: no
;dom: system x command (función)
;rec: system
(define run (lambda (system command) (command system)))

;descripción: Permite añadir una unidad de almacenamiento al sistema.
;recursión: no
;dom: system x letter (char) x name (string) x capacity (int)
;rec: system
(define add-drive (lambda (system)
                    (lambda (letter name capacity)
                      (if (equal? #f (buscar_drive (char-downcase letter) (unidades system)))
                      (armar_sistema (datos_sistema system)
                                (agregar_drive (unidades system) (drive (usuario_actual system) letter name capacity))
                                (usuarios system)
                                (papelera system))
                      system))))

;descripción: Función que permite registrar un usuario al sistema.
;recursión: no
;dom: system x userName
;rec: system
(define register (lambda (system)
                   (lambda (userName)
                     (armar_sistema (datos_sistema system)
                               (unidades system)
                               (if (equal? #t (buscar_usuario userName (usuarios system)))
                                   (usuarios system)
                                   (agregar_usuario (usuarios system) (user userName)))
                               (papelera system)))))

;descripción: Función que permite iniciar sesión con un usuario del sistema, solo si existe y no exista otra sesión iniciada.
;recursión: no
;dom: system x userName (String)
;rec: system
(define login (lambda (system)
                (lambda (userName)
                       (if (and (equal? "N/A" (usuario_actual system)) (equal? #t (buscar_usuario userName (usuarios system))))
                           (armar_sistema (modificar_user (car system) userName)
                                     (unidades system)
                                     (usuarios system)
                                     (papelera system))
                           system))))

;descripión: Función que permite iniciar sesión.
;recursión: no
;dom: system x userName (String)
;rec: system
(define logout (lambda (system)
                 (armar_sistema (modificar_user (car system) "N/A")
                           (unidades system)
                           (usuarios system)
                           (papelera system))))

;descripión: Función que permite fijar la unidad de almacenamiento en la que se realizarán las operaciones.
;recursión: no
;dom: system x letter (char)
;rec: system
(define switch-drive (lambda (system)
                       (lambda (letter)
                         (if (not (equal? "N/A" (usuario_actual system)))
                             (if (equal? #t (buscar_drive (char-downcase letter) (unidades system)))
                                 (armar_sistema (modificar_path (modificar_drive (car system) (char-downcase letter)) (list (string (char-downcase letter) #\:)))
                                           (ordenar_drives (unidades system) (char-downcase letter))
                                           (usuarios system)
                                           (papelera system))
                                 system)
                             system))))

;descripión: Función que permite crear un directorio dentro de la unidad con un nombre especificado.
;recursión: no
;dom: system x name (String)
;rec: system
(define md (lambda (system)
             (lambda (name)
                   (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) (string-downcase name) (car (ruta_actual system)))))
                       (armar_sistema (datos_sistema system)
                                 (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                                     (append (list (actualizar_fecha_modificacion (car (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
                                                             (cdr (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))))
                                             (list (folder (append (ruta_actual system) (list (string-downcase name))) (usuario_actual system)))))
                                     (cdr (unidades system)))
                             (usuarios system)
                             (papelera system))
                       system))))

;descripción: Función que permite cambiar la ruta (path) en que se realizan las operaciones.
;recursión: no
;dom: system x path or folderName (String)
;rec: system
(define cd (lambda (system)
             (lambda (path)
               (if (equal? path "/")
                   (volver_a_root system)
                   (if (equal? path "..")
                       (retroceder_carpeta system)
                       (if (equal? #f (comparar_rutas (rutas (carpetas_sistema (unidades system) null) null) (string-downcase path)))
                           (armar_sistema (modificar_path (car ((run system switch-drive) (string-ref (string-downcase path) 0))) (separar_string_ruta (string-downcase path)))
                                     (unidades ((run system switch-drive) (string-ref (string-downcase path) 0)))
                                     (usuarios system)
                                     (papelera system))
                           (if (equal? #f (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) (string-downcase path) (car (ruta_actual system)))))
                               (armar_sistema (modificar_path (car system) (append (ruta_actual system) (separar_string_ruta (string-downcase path))))
                                         (unidades system)
                                         (usuarios system)
                                         (papelera system))
                               system)))))))

;descripción: Función que añade un archivo a la ruta actual.
;recursión: no
;dom: system x file
;rec: system
(define add-file (lambda (system)
                   (lambda (file)
                     (armar_sistema (datos_sistema system)
                               (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                               (list (agregar_archivo_a_carpeta (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) file))
                                               (resto_carpetas (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
                                       (resto_unidades system))
                               (usuarios system)
                               (papelera system)))))

;descripción: Función que elimina un archivo o carpeta (con todos sus archivos y subdirectorios).
;recursión: no
;dom: system x fileName o folderName (String)
;rec: system
(define del (lambda (system)
              (lambda (name)
                (if (not (= 1 (length (separar_string (string-downcase name)))))
                  (if (equal? #f (buscar_archivos (nombres_archivos_unidad (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) null) (string-downcase name)))
                              (armar_sistema (datos_sistema system)
                                        (eliminar_archivo system (string-downcase name))
                                        (usuarios system)
                                        (append (papelera system) (list (list (ruta_actual system) (seleccionar_archivo (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) (string-downcase name))))))
                              system)
                  (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) (string-downcase name) (car (ruta_actual system)))))
                      system
                      (armar_sistema (datos_sistema system)
                                (append (list (append (list (caar (unidades system)))
                                        (list (cadar (unidades system)))
                                        (list (caddar (unidades system)))
                                        (eliminar_carpetas (append (list (append (actualizar_fecha_modificacion (carpeta_actual (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))))
                                                                     (archivos (carpeta_actual (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))))))
                                                                   (resto_carpetas (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))))
                                                           (append (list (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) (string-downcase name) (car (ruta_actual system))))))
                                                                   (filtrar_por_fuente (carpetas_unidad_actual system) (append (ruta_actual system) (list (string-downcase name))))))))
                                        (cdr (unidades system)))
                                (usuarios system)
                                (append (papelera system) (append (list (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) (string-downcase name) (car (ruta_actual system))))))
                                                                   (filtrar_por_fuente (carpetas_unidad_actual system) (append (ruta_actual system) (list (string-downcase name))))))))))))

;descripción: Función que elimina una carpeta que este vacía.
;recursión: no
;dom: system x folderName o folderPath (String)
;rec: system
(define rd (lambda (system)
             (lambda (name)
               (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) (string-downcase name) (car (ruta_actual system)))))
                   system
                   (if (and (equal? #f (tiene_archivos (car (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) (string-downcase name) (car (ruta_actual system)))))))
                            (equal? #f (tiene_carpetas (carpetas_unidad_actual system) (append (ruta_actual system) (list (string-downcase name))))))
                       (armar_sistema (datos_sistema system)
                                 (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                                     (cdr (primero_carpeta_actual (append (list (append (actualizar_fecha_modificacion (carpeta_actual (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))))
                                                                     (archivos (carpeta_actual (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))))))
                                                                   (resto_carpetas (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) (formar_ruta (cdr (ruta_actual system)) (string-downcase name) (car (ruta_actual system)))))))
                                       (resto_unidades system))
                                 (usuarios system)
                                 (papelera system))
                       system)))))

;descripción: Función que copia un archivo o carpeta desde una ruta origen a una ruta destino.
;recursión: no
;dom: system x source (file or folder) (String) x target path (String)
;rec: system
(define copy (lambda (system)
               (lambda (source target)
                 (if (not (= 1 (length (separar_string (string-downcase source)))))
                     (if (equal? #t (buscar_archivos (nombres_archivos_unidad (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) null) (string-downcase source)))
                     system
                     (if (equal? #t (comparar_rutas (rutas (carpetas_sistema (unidades system) null) null) (string-downcase target)))
                         system
                        (if (equal? #f (buscar_archivos (nombres_archivos_unidad (archivos (carpeta_actual (primero_carpeta_actual (carpetas (car (ordenar_drives (unidades system) (string-ref (string-downcase target) 0)))) (string-downcase target)))) null) (string-downcase source)))
                             system
                             (armar_sistema (datos_sistema system)
                                       (ordenar_drives (append (list (append (list (letra_unidad (car (ordenar_drives (unidades system) (string-ref (string-downcase target) 0))))
                                                     (nombre_unidad (car (ordenar_drives (unidades system) (string-ref (string-downcase target) 0))))
                                                     (size_unidad (car (ordenar_drives (unidades system) (string-ref (string-downcase target) 0)))))
                                               (list (agregar_archivo_a_carpeta (carpeta_actual (primero_carpeta_actual (carpetas (car (ordenar_drives (unidades system) (string-ref (string-downcase target) 0)))) (string-downcase target)))
                                                                                (seleccionar_archivo (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) (string-downcase source))))
                                               (resto_carpetas (primero_carpeta_actual (carpetas (car (ordenar_drives (unidades system) (string-ref (string-downcase target) 0)))) (string-downcase target)))))
                                               (cdr (ordenar_drives (unidades system) (string-ref (string-downcase target) 0)))) (string-ref (car (ruta_actual system)) 0))
                                   (usuarios system)
                                   (papelera system)))))
                     (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) (string-downcase source) (car (ruta_actual system)))))
                         system
                         (if (equal? #t (comparar_rutas (rutas (carpetas_sistema (unidades system) null) null) (string-downcase target)))
                             system
                             (if (equal? #f (comparar_rutas (rutas (carpetas_sistema (unidades system) null) null) (formar_ruta (cdr (separar_string_ruta (string-downcase target))) (string-downcase source) (car (separar_string_ruta (string-downcase target))))))
                                 system
                                 (armar_sistema (datos_sistema system)
                                 (ordenar_drives (cambiar_fecha (append (list (append (append (car (ordenar_drives (unidades system) (string-ref (string-downcase target) 0)))
                                                       (list (append (folder (append (separar_string_ruta (string-downcase target)) (list (string-downcase source))) (usuario_actual system))
                                                                     (archivos (carpeta_actual (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) (string-downcase source) (car (ruta_actual system)))))))))
                                         (nuevas_carpetas (carpetas_unidad_actual system) (append (ruta_actual system) (list (string-downcase source))) (string-downcase target))))
                                         (cdr (ordenar_drives (unidades system) (string-ref (string-downcase target) 0)))) target) (string-ref (car (ruta_actual system)) 0))
                                       (usuarios system)
                                       (papelera system)))))))))

;descripción: Función que mueve un archivo o carpeta desde una ruta origen a una ruta destino.
;recursión: no
;dom: system x source (file or folder) (String) x target path (String)
;rec: system
(define move (lambda (system)
               (lambda (source target)
                 (if (equal? ((run system copy) (string-downcase source) (string-downcase target)) system)
                     system
                     (armar_sistema (datos_sistema ((run ((run system copy) (string-downcase source) (string-downcase target)) del) (string-downcase source)))
                           (unidades ((run ((run system copy) (string-downcase source) (string-downcase target)) del) (string-downcase source)))
                           (usuarios system)
                           (papelera system))))))

;descripción: Función que permite renombrar una carpeta o archivo en el mismo nivel.
;recursión: no
;dom: system x currentName (String) x newName (String)
;rec: system
(define ren (lambda (system)
              (lambda (currentName newName)
                (if (= 1 (length (separar_string (string-downcase currentName))))
                    (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) (string-downcase currentName) (car (ruta_actual system)))))
                        system
                        (if (equal? #f (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) (string-downcase newName) (car (ruta_actual system)))))
                            system
                            (armar_sistema (datos_sistema system)
                                      (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                              (quitar_carpetas_antiguas (carpetas_unidad_actual system) (string-downcase newName) (length (append (ruta_actual system) (list (string-downcase currentName))))
                                                                        (append (list (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) (string-downcase currentName) (car (ruta_actual system))))))
                                                                                (filtrar_por_fuente (carpetas_unidad_actual system) (append (ruta_actual system) (list (string-downcase currentName))))))))
                                       (resto_unidades system))
                                      (usuarios system)
                                      (papelera system))))
                    (if (equal? #t (buscar_archivos (nombres_archivos_unidad (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) null) (string-downcase currentName)))
                        system
                        (if (equal? #f (buscar_archivos (nombres_archivos_unidad (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) null) (string-downcase newName)))
                            system
                            (armar_sistema (datos_sistema system)
                                  (cambiar_fecha (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                               (list (append (list (car (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
                                                     (append (list (modificar_nombre_archivo
                                                                    (car (primero_archivo_actual (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system))
                                                                                                                                                                            (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) (string-downcase currentName))) (string-downcase newName)))
                                                     (cdr (primero_archivo_actual (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) (string-downcase currentName))))))
                                               (resto_carpetas (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
                                               (resto_unidades system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))
                                  (usuarios system)
                                  (papelera system))))))))