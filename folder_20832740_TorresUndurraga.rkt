#lang racket
(require "system_20832740_TorresUndurraga.rkt")
(require "fecha_20832740_TorresUndurraga.rkt")
(require "drive_20832740_TorresUndurraga.rkt")
(provide (all-defined-out))

;Implementación del TDA folder

#|REPRESENTACIÓN: Este TDA representa una folder (carpeta) y sus datos.
   Una lista de listas con la dirección de la carpeta como una lista de strings,
el nombre del usuario que la creo como un string, la fecha de creación como el
TDA fecha, la fecha de modificación como del TDA fecha y los atributos de
seguridad como una lista de char.|#
;CONSTRUCTOR

;descripción: Función que permite crear un folder (carpeta).
;recursión: no
;dom: dirección (nombre) x creador (String) x seguridad (Lista de char)
;rec: folder
(define folder (lambda (direccion creador . seguridad)
                  (list (list direccion creador (fecha) (fecha) (if (not (null? seguridad))
                                                                    seguridad
                                                                    (list null))))))

;SELECTORES

(define carpetas cdddr) ;selecciona las carpetas de una unidad

(define carpeta_actual car) ;selecciona la carpeta en la que se realizan las operaciones

(define ruta_carpeta caar) ;selecciona la ruta de una carpeta

(define root (lambda (system)
               (list (car (ruta_actual system))))) ;selecciona la dirección del root de la unidad

(define carpetas_unidad_actual (lambda (system)
                                 (cdddar (unidades system)))) ;selecciona las carpetas de la unidad en la que se realizan las operaciones

(define resto_carpetas cdr) ;selecciona las carpetas en las que no se realizan las operaciones

(define nombre_carpeta cadaar) ;selecciona el nombre de la carpeta

(define atributos_seguridad_carpeta (lambda (carpeta) (car (list-ref (car carpeta) 4))))

;MODIFICADORES
                                       
;descripción: Función que elimina las carpetas a remover una lista de carpetas.
;recursión: sí, recursión natural, porque se elimina en secuencia, una a una hasta que la lista sea nulo.
;dom: carpetas (lista de folder) x newName (String) x posicion (integer) x carpetas_a_remover (lista de folder)
;rec: carpetas (actualizada)
(define quitar_carpetas_antiguas (lambda (carpetas newName posicion carpetas_a_remover)
                                   (if (null? carpetas_a_remover)
                                       carpetas
                                       (quitar_carpetas_antiguas (append (remove (car carpetas_a_remover) carpetas)
                                                                         (list (append (list (list (modificar_posicion_ruta (cdr (ruta_carpeta (car carpetas_a_remover)))
                                                                                                                               (list (car (ruta_carpeta (car carpetas_a_remover))))
                                                                                                                               (- posicion 2) newName)
                                                                                                   (list-ref (caar carpetas_a_remover) 1)
                                                                                                   (list-ref (caar carpetas_a_remover) 2)
                                                                                                   (if (= posicion (length (ruta_carpeta (car carpetas_a_remover)))) (fecha)
                                                                                                       (list-ref (caar carpetas_a_remover) 3))
                                                                                                   (list-ref (caar carpetas_a_remover) 4)))
                                                                                       (cdr (car carpetas_a_remover)))))
                                                                         newName posicion (cdr carpetas_a_remover)))))

;descripción: Función que cambia el directorio actual a root.
;recursión: no
;dom: system
;rec: system
(define volver_a_root (lambda (system)
                        (list (modificar_path (car system) (root system))
                                  (unidades system)
                                  (usuarios system)
                                  (papelera system))))

;descripción: Función que modifica el nombre de un archivo.
;recursión: no
;dom: archivo x newName
;rec: archivo
(define modificar_nombre_archivo (lambda (archivo newName)
                                   (list newName
                                         (cadr (separar_string newName))
                                         (list-ref archivo 2)
                                         (list-ref archivo 3))))

;descripción: Función que quita la última carpeta del directorio actual.
;recursión: no
;dom: path (list)
;rec: path (list)
(define remover_carpeta_final (lambda (path)
                                (if (= 1 (length path))
                                    path
                                    (reverse (cdr (reverse path))))))

;descripción: Función que retrocede a la carpeta anterior del directorio actual.
;recursión: no
;dom: system
;rec: system
(define retroceder_carpeta (lambda (system)
                             (list (modificar_path (car system) (remover_carpeta_final (ruta_actual system)))
                                       (unidades system)
                                       (usuarios system)
                                       (papelera system))))

;descripción: Función que modifica la ruta de una carpeta en una posición específica.
;recursión: sí, recursión natural, porque recorre la lista de la ruta hasta llegar a la posición ingresada.
;dom: lista1 x lista2 x posicion x reemplazo
;rec: lista2 (ruta de modificada)
(define modificar_posicion_ruta (lambda (lista1 lista2 posicion reemplazo)
                             (if (= 0 posicion)
                                 (append lista2 (append (list reemplazo) (cdr lista1)))
                                 (modificar_posicion_ruta (cdr lista1) (append lista2 (list (car lista1))) (- posicion 1) reemplazo))))

;descripción: Función que elimina carpetas de una lista de carpetas.
;recursión: sí, recursión natural, porque recorre la lista de carpetas a eliminar una a una.
;dom: carpetas (lista de carpetas) x carpetas_a_eliminar (lista de carpetas que se deben eliminar)
;rec: carpetas (lista de carpetas)
(define eliminar_carpetas (lambda (carpetas carpetas_a_eliminar)
                            (if (null? carpetas_a_eliminar)
                                carpetas
                                (eliminar_carpetas (remove (car carpetas_a_eliminar) carpetas) (cdr carpetas_a_eliminar)))))

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

;descripción: Función que busca una carpeta en la primera unidad y cambia su fecha de modificación.
;recursión: no
;dom: drives x target (String)
;rec: drives (actualizado)
(define cambiar_fecha (lambda (drives target)
                               (append (list (append (list (letra_unidad (car drives))
                                             (nombre_unidad (car drives))
                                             (size_unidad (car drives)))
                                                     (list (append (actualizar_fecha_modificacion (carpeta_actual (primero_carpeta_actual (carpetas (car drives)) target)))
                                                     (cdr (carpeta_actual (primero_carpeta_actual (carpetas (car drives)) target)))))
                                                     (resto_carpetas (primero_carpeta_actual (carpetas (car drives)) target))))
                                       (cdr drives))))

;OTRAS OPERACIONES

;descripción: Función que forma una ruta como string. 
;recursión: sí, recursión natural, porque se recorre la lista de path y se agregan a name para formar el string de la ruta.
;dom: path (lista con la ruta) x folder_name (String) x name (lista con el primer string de la ruta)
;rec: name
(define formar_ruta (lambda (path folder_name name)
                      (if (null? path)
                          (if (equal? "" folder_name)
                              (string-append name "/")
                              (string-append name "/" folder_name "/"))
                          (formar_ruta (cdr path) folder_name (string-append name "/" (car path))))))

;descripción: Función que reordena las carpetas para dejar primero la carpeta en la que se realizan las operaciones.
;recursión: sí, recursión natural, porque se reordenan las carpetas hasta que la primera tenga el nombre de la ingresada.
;dom: carpetas (lista de folders) x nombre (String)
;rec: carpetas (lista de folders ordenada)
(define primero_carpeta_actual (lambda (carpetas nombre)
                         (if (equal? nombre (formar_ruta (cdr (ruta_carpeta (car carpetas))) "" (car (ruta_carpeta (car carpetas)))))
                             carpetas
                             (primero_carpeta_actual (append (cdr carpetas) (list (car carpetas))) nombre))))

;descripción: Función que camvia la fuente de una lista de carpetas.
;recursión: no
;dom: carpetas x path x path_objetivo
;rec: carpetas (con una nueva fuente)
(define nuevas_carpetas (lambda (carpetas path path_objetivo)
                          (map (lambda (x) (nueva_fuente x path_objetivo)) (subcarpetas carpetas path))))

;descripción: Función que separa un string en los ".".
;recursión: no
;dom: name (String)
;rec: name (lista de string)
(define separar_string (lambda (name)
                         (string-split name "." #:trim? #t)))

;descripción: Función que separa un string en los "/".
;recursión: no
;dom: name
;rec: name (lista de string)
(define separar_string_ruta (lambda (name)
                         (string-split name "/" #:trim? #t)))

;descripción: Función que avanza en 1 posición en la ruta (directorio) de una carpeta.
;recursión: no
;dom: ruta (lista)
;rec: ruta (lista actualizada)
(define avanzar_directorio_carpeta (lambda (ruta)
                                     (append (list (append (list (cdaar ruta)) (cdar ruta))) (cdr ruta))))

;descripción: Función que recopila todas las carpetas del sistema.
;recursión: sí, recursión natural, porque recorre todas las unidades y extrae las carpetas.
;dom: unidades (lista de drives) x lista (lista con carpetas)
;rec: lista (lista con carpetas)
(define carpetas_sistema (lambda (unidades lista)
                           (if (null? unidades)
                               lista
                               (carpetas_sistema (cdr unidades) (append lista (cdddar unidades))))))

;descripción: Función que forma como string y muestra todas las rutas de carpetas de la unidad.
;recursión: sí, recursión natural, porque recorre una a una las carpetas y las agrega a la lista.
;dom: carpetas (list) x lista (list)
;rec: lista (list)
(define rutas (lambda (carpetas lista)
                (if (null? carpetas)
                    lista
                    (rutas (cdr carpetas) (append lista (list (formar_ruta (cdr (ruta_carpeta (car carpetas))) "" (car (ruta_carpeta (car carpetas))))))))))

;descripción: Función que cambia la ruta fuente de una carpeta.
;recursión: no
;dom: carpeta x path_objetivo
;rec: carpeta (ruta fuente actualizada)
(define nueva_fuente (lambda (carpeta path_objetivo)
          (if (null? (cdr carpeta))
              (list (append (list (append (separar_string_ruta path_objetivo) (caar carpeta))) (cdar carpeta)))
              (list (append (list (append (separar_string_ruta path_objetivo) (caar carpeta))) (cdar carpeta)) (cdr carpeta)))))

;descripción: Función que filtra las carpetas que tengan la misma fuente que la ruta ingresada.
;recursión: no
;dom: carpetas (lista de folders) x path ()
;rec: carpetas (filtrada)
(define filtrar_por_fuente (lambda (carpetas path)
                         (filter (lambda (x) (igual_fuente_eliminar x path)) (comparar_largo_rutas carpetas path null))))

;descripción: Función que recolecta todas las subcarpetas de una ruta.
;recursión: no
;dom: carpetas x path
;rec: carpetas (solo las subcarpetas)
(define subcarpetas (lambda (carpetas path)
                      (map (lambda (x) (carpeta_igual_fuente x path)) (filtrar_por_fuente carpetas path))))

;descripción: Función que modifica la ruta de una carpeta hasta eliminar el path de esta.
;recursión: sí, recursión natural, porque recorre y compara uno a uno path_carpeta y path.
;dom: path_carpeta x path
;rec: path_carpeta
(define carpeta_igual_fuente (lambda (path_carpeta path)
                       (if (= 1 (length path))
                           path_carpeta
                           (carpeta_igual_fuente (avanzar_directorio_carpeta path_carpeta) (cdr path)))))

;descripción: Función que compara si la ruta de las carpetas tienen mayor tamaño que la ingresada.
;recursión: sí, recursión natural, porque recorre la lista de carpetas y quita las que no cumplen la condición.
;dom: carpetas (lista de folders) x path (ruta) x lista (lista con las carpetas)
;rec: lista
(define comparar_largo_rutas (lambda (carpetas path lista)
                   (if (null? carpetas)
                       lista
                       (if (> (length (caaar carpetas)) (length path))
                           (comparar_largo_rutas (cdr carpetas) path (append lista (list (car carpetas))))
                           (comparar_largo_rutas (cdr carpetas) path lista)))))

;descripción: Función que que compara y guarda si es que el largo de una ruta es menor o igual al ingresado.
;recursión: sí, recursión natural, porque recorre las rutas una a una y elimina las que no cumplan la condición.
;dom: carpetas x path x lista
;rec: lista
(define comparar (lambda (carpetas path lista)
                   (if (null? carpetas)
                       lista
                       (if (> (length (caaar carpetas)) (length path))
                           (comparar (cdr carpetas) path (append lista (list (caaar carpetas))))
                           (comparar (cdr carpetas) path lista)))))

;descripción: Función que que determina si una carpeta tiene una ruta de igual fuente que la ingresada.
;recursión: sí, recursión natural, porque recorre la ruta de una carpeta y compara una a una. 
;dom: path_carpeta x path
;rec: booleano
(define igual_fuente_eliminar (lambda (path_carpeta path)
                       (if (null? path)
                           #t
                           (if (equal? (caaar path_carpeta) (car path))
                               (igual_fuente_eliminar (avanzar_directorio_carpeta path_carpeta) (cdr path))
                               #f))))

;descripción: Función que define si una carpeta (ruta) ya existe.
;recursión: no
;dom: rutas (list) x path (String)
;rec: booleano
(define comparar_rutas (lambda (rutas path)
                         (if (null? (filter (lambda (rutas) (equal? rutas path)) rutas))
                             #t
                             #f)))

;descripción: Función que determina si una carpeta tiene archivos dentro.
;recursión: no
;dom: carpeta
;rec: booleano
(define tiene_archivos (lambda (carpeta)
                         (if (null? (cdr carpeta))
                             #f
                             #t)))

;descripción: Función que determina si una carpeta tiene carpetas dentro.
;recursión: no
;dom: carpetas x path
;rec: booleano
(define tiene_carpetas (lambda (carpetas path)
                         (if (null? (filter (lambda (x) (igual_fuente x path)) (comparar carpetas path null)))
                             #f
                             #t)))

;descripción: Función que determina si una ruta tiene igual fuente que la ingresada.
;recursión: sí, recursión natural, porque compara una a una ambas rutas hasta que se vuelva nula.
;dom: path_carpeta x path
;rec: booleano
(define igual_fuente (lambda (path_carpeta path)
                       (if (null? path)
                           #t
                           (if (equal? (car path_carpeta) (car path))
                               (igual_fuente (cdr path_carpeta) (cdr path))
                               #f))))

;descripción: Función que extrae los nombres de las carpetas y los deja en una lista.
;recursión: sí, recursión natural, porque comprueba si el nombre existe para cada carpeta.
;dom: carpetas x path
;rec: carpetas (solo los nombres)
(define nombres_carpetas_no_ocultas (lambda (carpetas lista)
                           (if (null? carpetas)
                               lista
                               (if (and (equal? #f (member #\h (atributos_seguridad_carpeta (car carpetas)))) (equal? #f (member (nombre_carpeta (car carpetas)) lista)))
                                   (nombres_carpetas_no_ocultas (cdr carpetas) (append lista (list (nombre_carpeta (car carpetas)))))
                                   (nombres_carpetas_no_ocultas (cdr carpetas) lista)))))

;descripción: Función que extrae los nombres de las carpetas y los deja en una lista.
;recursión: sí, recursión natural, porque comprueba si el nombre existe para cada carpeta.
;dom: carpetas x path
;rec: carpetas (solo los nombres)
(define nombres_carpetas (lambda (carpetas lista)
                           (if (null? carpetas)
                               lista
                               (if (equal? #f (member (nombre_carpeta (car carpetas)) lista))
                                   (nombres_carpetas (cdr carpetas) (append lista (list (nombre_carpeta (car carpetas)))))
                                   (nombres_carpetas (cdr carpetas) lista)))))

;descripción: Función que extrae los nombres de las carpetas y los deja en una lista.
;recursión: sí, recursión natural, porque comprueba si el nombre existe para cada carpeta.
;dom: carpetas x path
;rec: carpetas (solo los nombres)
(define nombres_carpetas_y_subcarpetas_no_ocultas (lambda (carpetas lista)
                           (if (null? carpetas)
                               lista
                               (if (equal? #f (member #\h (atributos_seguridad_carpeta (car carpetas))))
                                   (nombres_carpetas_y_subcarpetas_no_ocultas (cdr carpetas) (append lista (list (if (= 1 (length (cdaar (car carpetas))))
                                                                                                                     (nombre_carpeta (car carpetas))
                                                                                                                     (formar_ruta (cddaar (car carpetas)) "" (cadaar (car carpetas)))))))
                                   (nombres_carpetas_y_subcarpetas_no_ocultas (cdr carpetas) lista)))))

;descripción: Función que extrae los nombres de las carpetas y los deja en una lista.
;recursión: sí, recursión natural, porque comprueba si el nombre existe para cada carpeta.
;dom: carpetas x path
;rec: carpetas (solo los nombres)
(define nombres_carpetas_y_subcarpetas (lambda (carpetas lista)
                           (if (null? carpetas)
                               lista
                                   (nombres_carpetas_y_subcarpetas (cdr carpetas) (append lista (list (if (= 1 (length (cdaar (car carpetas))))
                                                                                                                     (nombre_carpeta (car carpetas))
                                                                                                                     (formar_ruta (cddaar (car carpetas)) "" (cadaar (car carpetas))))))))))

;descripción: Función que forma el string que contiene el contenido de un directorio.
;recursión: sí, recursión natural, porque agrega cada elemento de las listas al string final.
;dom: string (String) x nombres_carpetas (lista de strings) x archivos (lista de strings)
;rec: string
(define formar_string (lambda (string nombres_carpetas nombres_archivos)
                        (if (null? nombres_carpetas)
                            (if (null? nombres_archivos)
                                string
                                (formar_string (string-append string "\n" (car nombres_archivos)) nombres_carpetas (cdr nombres_archivos)))
                            (formar_string (string-append string "\n" (car nombres_carpetas)) (cdr nombres_carpetas) nombres_archivos))))