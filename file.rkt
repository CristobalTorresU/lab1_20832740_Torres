#lang racket
(require "drive.rkt" "folder.rkt" "fecha.rkt" "user.rkt")
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
                                                             (carpeta_actual (primero_carpeta_actual (carpetas (unidad_actual system)) (formar_ruta (cdr (ruta_actual system)) "" (car (ruta_actual system)))))))
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

;descripción: Función que actualiza la fecha de modificación de una carpeta.
;recursión: no
;dom: carpeta (folder)
;rec: folder
(define actualizar_fecha_modificacion (lambda (carpeta)
                                        (list (list (list-ref (car carpeta) 0)
                                                    (list-ref (car carpeta) 1)
                                                    (list-ref (car carpeta) 2)
                                                    (fecha)
                                                    (list-ref (car carpeta) 4)))))

;OTRAS OPERACIONES

;descripción: Función que reordena los archivos para dejar primero el archivo que se quiere modificar.
;recursión: sí, recursión natural, porque
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