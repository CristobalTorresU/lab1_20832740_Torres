#lang racket
(require "system.rkt")
(require "drive.rkt")
(require "user.rkt")
(require "folder.rkt")
(require "file.rkt")
(require "fecha.rkt")

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
                      (list (datos_sistema system)
                                (agregar_drive (unidades system) (drive system letter name capacity))
                                (usuarios system)
                                (papelera system))
                      system))))

;descripción: Función que permite registrar un usuario al sistema.
;recursión: no
;dom: system x userName
;rec: system
(define register (lambda (system)
                   (lambda (userName)
                     (list (datos_sistema system)
                               (unidades system)
                               (if (equal? #t (buscar_usuario userName (usuarios system)))
                                   (usuarios system)
                                   (agregar_usuario (usuarios system) (user userName)))
                               (papelera system)))))

;descripción: Función que permite iniciar sesión con un usuario del sistema.
;recursión: no
;dom: system x userName (String)
;rec: system
(define login (lambda (system)
                (lambda (userName)
                       (if (equal? #t (buscar_usuario userName (usuarios system)))
                           (list (modificar_user (car system) userName)
                                     (unidades system)
                                     (usuarios system)
                                     (papelera system))
                           system))))

;descripión: Función que permite iniciar sesión.
;recursión: no
;dom: system x userName (String)
;rec: system
(define logout (lambda (system)
                 (list (modificar_user (car system) "N/A")
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
                                 (list (modificar_path (modificar_drive (car system) (char-downcase letter)) (list (string (char-downcase letter) #\:)))
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
                   (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))
                       (list (datos_sistema system)
                                 (append (list (append (car (unidades system))
                                             (list (carpeta (append (ruta_actual system) (list name)) (usuario_actual system)))))
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
                           (list (modificar_path (car ((run system switch-drive) (string-ref (string-downcase path) 0))) (separar_string_ruta (string-downcase path)))
                                     (unidades ((run system switch-drive) (string-ref (string-downcase path) 0)))
                                     (usuarios system)
                                     (papelera system))
                           (if (equal? #f (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) path (car (ruta_actual system)))))
                               (list (modificar_path (car system) (append (ruta_actual system) (separar_string_ruta path)))
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
                     (list (datos_sistema system)
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
                (if (not (= 1 (length (separar_string name))))
                  (if (equal? #f (buscar_archivos (nombres_archivos_unidad (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) null) name))
                              (list (datos_sistema system)
                                        (eliminar_archivo system name)
                                        (usuarios system)
                                        (append (papelera system) (list (seleccionar_archivo (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) name))))
                              system)
                  (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))
                      system
                      (list (datos_sistema system)
                                (append (list (append (list (caar (unidades system)))
                                        (list (cadar (unidades system)))
                                        (list (caddar (unidades system)))
                                        (eliminar_carpetas (carpetas_unidad_actual system) (append (list (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))) (tiene_carpetas2 (carpetas_unidad_actual system) (append (ruta_actual system) (list name)))))))
                                        (cdr (unidades system)))
                                (usuarios system)
                                (papelera system)))))))

;descripción: Función que elimina una carpeta que este vacía.
;recursión: no
;dom: system x folderName o folderPath (String)
;rec: system
(define rd (lambda (system)
             (lambda (name)
               (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))
                   system
                   (if (and (equal? #f (tiene_archivos (car (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system))))))) (equal? #f (tiene_carpetas (carpetas_unidad_actual system) (append (ruta_actual system) (list name)))))
                       (list (datos_sistema system)
                                 (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                                     (cdr (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) name (car (ruta_actual system)))))))
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
                 (if (not (= 1 (length (separar_string source))))
                     (if (equal? #t (buscar_archivos (nombres_archivos_unidad (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) null) source))
                     system
                     (if (equal? #t (comparar_rutas (rutas (carpetas_sistema (unidades system) null) null) target))
                         system
                        (if (equal? #f (buscar_archivos (nombres_archivos_unidad (archivos (carpeta_actual (primero_carpeta_actual (carpetas (car (ordenar_drives (unidades system) (string-ref target 0)))) target))) null) source))
                             system
                             (list (datos_sistema system)
                                       (ordenar_drives (append (list (append (list (letra_unidad (car (ordenar_drives (unidades system) (string-ref target 0))))
                                                     (nombre_unidad (car (ordenar_drives (unidades system) (string-ref target 0))))
                                                     (size_unidad (car (ordenar_drives (unidades system) (string-ref target 0)))))
                                               (list (agregar_archivo_a_carpeta (carpeta_actual (primero_carpeta_actual (carpetas (car (ordenar_drives (unidades system) (string-ref target 0)))) target)) (seleccionar_archivo (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) source)))
                                               (resto_carpetas (primero_carpeta_actual (carpetas (car (ordenar_drives (unidades system) (string-ref target 0)))) target))))
                                               (cdr (ordenar_drives (unidades system) (string-ref target 0)))) (string-ref (car (ruta_actual system)) 0))
                                       (usuarios system)
                                       (papelera system)))))
                     (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) source (car (ruta_actual system)))))
                         system
                         (if (equal? #t (comparar_rutas (rutas (carpetas_sistema (unidades system) null) null) target))
                             system
                             (if (equal? #f (comparar_rutas (rutas (carpetas_sistema (unidades system) null) null) (formar_ruta (cdr (separar_string_ruta target)) source (car (separar_string_ruta target)))))
                                 system
                                 (list (datos_sistema system)
                                 (ordenar_drives (append (list (append (car (ordenar_drives (unidades system) (string-ref target 0)))
                                                       (list (append (carpeta (append (separar_string_ruta target) (list source)) (usuario_actual system)) (archivos (carpeta_actual (primero_carpeta_actual (carpetas_unidad_actual system) (formar_ruta (cdr (ruta_actual system)) source (car (ruta_actual system))))))))))
                                         (nuevas_carpetas (carpetas_unidad_actual system) (append (ruta_actual system) (list source)) target)
                                         (cdr (ordenar_drives (unidades system) (string-ref target 0)))) (string-ref (car (ruta_actual system)) 0))
                                 (usuarios system)
                                 (papelera system)))))))))

;descripción: Función que mueve un archivo o carpeta desde una ruta origen a una ruta destino.
;recursión: no
;dom: system x source (file or folder) (String) x target path (String)
;rec: system
(define move (lambda (system)
               (lambda (source target)
                 (if (equal? ((run system copy) source target) system)
                     system
                     (list (datos_sistema ((run ((run system copy) source target) del) source))
                               (unidades ((run ((run system copy) source target) del) source))
                               (usuarios ((run ((run system copy) source target) del) source))
                               (papelera ((run system copy) source target)))))))

;descripción: Función que permite renombrar una carpeta o archivo en el mismo nivel.
;recursión: no
;dom: system x currentName (String) x newName (String)
;rec: system
(define ren (lambda (system)
              (lambda (currentName newName)
                (if (= 1 (length (separar_string currentName)))
                    (if (equal? #t (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) currentName (car (ruta_actual system)))))
                        system
                        (if (equal? #f (comparar_rutas (rutas (carpetas_unidad_actual system) null) (formar_ruta (cdr (ruta_actual system)) newName (car (ruta_actual system)))))
                            system
                            (list (datos_sistema system)
                                      (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                              (quitar_carpetas_antiguas (carpetas_unidad_actual system) newName (length (append (ruta_actual system) (list currentName))) (append (list (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) currentName (car (ruta_actual system)))))) (tiene_carpetas2 (carpetas_unidad_actual system) (append (ruta_actual system) (list currentName)))))))
                                       (resto_unidades system))
                                      (usuarios system)
                                      (papelera system))))
                    (if (equal? #t (buscar_archivos (nombres_archivos_unidad (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) null) currentName))
                        system
                        (if (equal? #f (buscar_archivos (nombres_archivos_unidad (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) null) newName))
                            system
                            (list (datos_sistema system)
                                       (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                               (list (append (list (car (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
                                                     (append (list (modificar_nombre_archivo (car (primero_archivo_actual (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) currentName)) newName))
                                                     (cdr (primero_archivo_actual (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))) currentName)))))
                                               (resto_carpetas (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
                                               (resto_unidades system))
                                       (usuarios system)
                                       (papelera system))))))))