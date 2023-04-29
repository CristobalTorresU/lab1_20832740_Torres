#lang racket
(require "system_20832740_TorresUndurraga.rkt")
(require "drive_20832740_TorresUndurraga.rkt")
(require "folder_20832740_TorresUndurraga.rkt")
(provide (all-defined-out))

;Implementación del TDA file

#|REPRESENTACIÓN: Este TDA representa un archivo (file) y sus datos pertinentes.
   Una lista con el nombre del archivo, tipo de archivo, contenido del archivo.
y atributos de seguridad; donde el primer, segundo y tercer elemento es de tipo
String y los atributos de seguridad se encuentran en una lista, y estos son de tipo
char.|#
;CONSTRUCTORES
;descripción: Función que crea un archivo.
;recursión: no
;dom: nombre (String) x tipo_archivo (String) x contenido (String)
;rec: file
(define file (lambda (nombre tipo_archivo contenido . seguridad)
               (list (string-downcase nombre) (string-downcase tipo_archivo) contenido seguridad)))

;SELECTORES

;descripción: Función que selecciona un archivo dentro de un directorio.
;recursión: sí, recursión natural, porque necesita recorrer los archivos hasta encontrar el que requiere.
;dom: archivos (List) x name (String)
;rec: file
(define seleccionar_archivo (lambda (archivos name)
                                  (if (equal? name (caar archivos))
                                      (car archivos)
                                      (seleccionar_archivo (cdr archivos) name))))

(define nombre_archivo car) ;selecciona el nombre de un archivo

(define atributos_seguridad_archivo cadddr)

(define archivos cdr) ;selecciona los archivos de una carpeta

;descripción: Función que crea una lista con los nombres de los archivos que se encuentran en la carpeta actual.
;recursión: sí, recursión natural, porque recorre todos los archivos y recolecta sus nombres.
;dom: archivos (lista de files) x list
;rec: lista (contiene los nombres de los archivos)
(define nombres_archivos_unidad (lambda (archivos lista)
                                  (if (null? archivos)
                                      lista
                                      (nombres_archivos_unidad (cdr archivos)
                                                               (append lista (list (nombre_archivo (car archivos))))))))

;MODIFICADORES

;descripción: Función que elimina un archivo en particular de una unidad.
;recursión: no
;dom: system x name (String)
;rec: archivos (Lista de files)
(define eliminar_archivo (lambda (system name)
                           (append (list (append (list (letra_unidad (unidad_actual system))
                                                     (nombre_unidad (unidad_actual system))
                                                     (size_unidad (unidad_actual system)))
                                               (list (remove (seleccionar_archivo (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))) name)
                                                             (append (actualizar_fecha_modificacion (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system))))))
                                                                     (archivos (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))))
                                               (resto_carpetas (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
                                       (resto_unidades system))))

;descripción: Función que permite agregar un archivo a una carpeta.
;recursión: no
;dom: carpeta x file
;rec: carpeta
(define agregar_archivo_a_carpeta (lambda (carpeta file)
                                    (if (or (null? (archivos carpeta)) (equal? #t (buscar_archivos (nombres_archivos_unidad (archivos carpeta) null) (nombre_archivo file))))
                                        (append (actualizar_fecha_modificacion carpeta) (cdr carpeta) (list file))
                                        carpeta)))

;OTRAS OPERACIONES

;descripción: Función que reordena los archivos para dejar primero el archivo que se quiere modificar.
;recursión: sí, recursión natural, porque cambia la posición del primer archivo hasta que sea el ingresado.
;dom: archivos (lista de archivos) x name (String)
;rec: archivos (lista de archivos)
(define primero_archivo_actual (lambda (archivos name)
                                 (if (equal? name (nombre_archivo (car archivos)))
                                     archivos
                                     (primero_archivo_actual (append (cdr archivos) (list (car archivos))) name))))

;descripción: Función que avanza en la ruta de una carpeta.
;recursión: no
;dom: ruta
;rec: ruta (actualizada)
(define avanzar_ruta_carpeta (lambda (ruta)
                                     (append (list (append (list (cdaar ruta)) (cdar ruta))) (cdr ruta))))

;descripción: Función que indica si el nombre de un archivo existe en una carpeta.
;recursión: no
;dom: archivos (lista de files) x file_name (String)
;rec: booleano
(define buscar_archivos (lambda (archivos file_name)
                         (if (null? (filter (lambda (archivos) (equal? archivos file_name)) archivos))
                             #t
                             #f)))

;descripción: Función que recolecta nombres de los archivos que no estén ocultos.
;recursión: sí, recursión natural, porque revisa uno a uno los nombres para que no se repitan y los parámetros de seguridad para saber si es un archivo oculto.
;dom: archivos x lista
;rec: lista
(define nombres_archivos_no_ocultos (lambda (archivos lista)
                                      (if (null? archivos)
                                          lista
                                          (if (and (equal? #f (member #\h (atributos_seguridad_archivo (car archivos)))) (equal? #f (member (nombre_archivo (car archivos)) lista)))
                                              (nombres_archivos_no_ocultos (cdr archivos) (append lista (list (nombre_archivo (car archivos)))))
                                              (nombres_archivos_no_ocultos (cdr archivos) lista)))))

;descripción: Función que recolecta el nombre de los archivos.
;recursión: sí, recursión natural, porque compara uno a uno los nombres de los archivos para verificar que no se repitan.
;dom: archivos x lista
;rec: lista
(define nombres_archivos (lambda (archivos lista)
                                      (if (null? archivos)
                                          lista
                                          (if (equal? #f (member (nombre_archivo (car archivos)) lista))
                                              (nombres_archivos (cdr archivos) (append lista (list (nombre_archivo (car archivos)))))
                                              (nombres_archivos (cdr archivos) lista)))))

;descripción: Función que ordena alfabeticamente el string de la función dir de forma ascendete o descendente.
;recursión: no 
;dom: archivos x lista
;rec: lista
(define ordenar_alfabeticamente (lambda (opcion lista)
                                  (if (equal? #\- (string-ref opcion 3))
                                      (sort lista string>?)
                                      (sort lista string<?))))

;descripción: Función que forma el string que contiene el contenido de un directorio y sus subdirectorios.
;recursión: sí, recursión natural, porque agrega cada elemento de las listas al string final.
;dom: string (String) x nombres_carpetas (lista de strings) x archivos (lista de strings)
;rec: string
(define formar_string_con_subdirectorios (lambda (string subcarpetas names_files)
                        (if (null? names_files)
                            (if (null? subcarpetas)
                                string
                                (formar_string_con_subdirectorios (string-append string "\n\n" (if (= 1 (length (cdaar (car subcarpetas)))) (nombre_carpeta (car subcarpetas)) (formar_ruta (cddaar (car subcarpetas)) "" (cadaar (car subcarpetas))))) (cdr subcarpetas) (nombres_archivos (archivos (car subcarpetas)) null)))
                            (formar_string_con_subdirectorios (string-append string "\n" (car names_files)) subcarpetas (cdr names_files)))))

;descripción: Función que forma el string que contiene el contenido de un directorio y sus subdirectorios que no estén ocultos.
;recursión: sí, recursión natural, porque agrega cada elemento de las listas al string final.
;dom: string (String) x nombres_carpetas (lista de strings) x archivos (lista de strings)
;rec: string
(define formar_string_con_subdirectorios_no_ocultos (lambda (string subcarpetas names_files)
                        (if (null? names_files)
                            (if (null? subcarpetas)
                                string
                                (if (equal? #f (member #\h (atributos_seguridad_carpeta (car subcarpetas))))
                                    (formar_string_con_subdirectorios_no_ocultos (string-append string "\n\n" (if (= 1 (length (cdaar (car subcarpetas))))
                                                                                                                  (nombre_carpeta (car subcarpetas))
                                                                                                                  (formar_ruta (cddaar (car subcarpetas)) "" (cadaar (car subcarpetas)))))
                                                                             (cdr subcarpetas) (nombres_archivos_no_ocultos (archivos (car subcarpetas)) null))
                                    (formar_string_con_subdirectorios_no_ocultos string (cdr subcarpetas) names_files)))
                            (formar_string_con_subdirectorios_no_ocultos (string-append string "\n" (car names_files)) subcarpetas (cdr names_files)))))