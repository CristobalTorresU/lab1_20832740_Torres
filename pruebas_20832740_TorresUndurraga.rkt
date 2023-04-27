#lang racket
(require "main_20832740_TorresUndurraga.rkt")
(require "system_20832740_TorresUndurraga.rkt")
(require "drive_20832740_TorresUndurraga.rkt")
(require "user_20832740_TorresUndurraga.rkt")
(require "folder_20832740_TorresUndurraga.rkt")
(require "file_20832740_TorresUndurraga.rkt")

;SCRIPT DE PRUEBAS
;creando un sistema
(define S0 (system "newSystem"))
(define S0.1 (system "newSystem_2"))
(define S0.2 (system "newSystem_3"))
(define S0.3 (system "Unix-like"))

;añadiendo unidades. Incluye caso S2 que intenta añadir unidad con una letra que ya existe
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))
(define S4 ((run S3 add-drive) #\e "Media" 8000))
(define S5 ((run S4 add-drive) #\A "UEFI" 256))
(define S6 ((run S5 add-drive) #\f "Media" 8000)) #|Siempre y cuando la letra sea distinta,
puede tener igual nombre y tamaño.|#

;añadiendo usuarios. Incluye caso S6 que intenta registrar usuario duplicado
(define S7 ((run S6 register) "user1"))
(define S8 ((run S7 register) "user1"))
(define S9 ((run S8 register) "user2"))
(define S10 ((run S9 register) "newUser"))
(define S11 ((run S10 register) "guest"))

;iniciando sesión con usuarios. Incluye caso S8 que intenta iniciar sesión con user2 sin antes haber salido con user1
(define S12 ((run S11 login) "user1"))
(define S13 ((run S12 login) "user2"))

;cerrando sesión user1 e iniciando con user2
(define S14 (run S13 logout))
(define S15 ((run S14 login) "user2"))

;registrando otro usuario y cerrando sesión
(define S16 ((run S15 register) "admin"))
(define S17 (run S16 logout))

;se inicia sesión con el nuevo usuario y luego se intenta iniciar nuevamente la sesión del mismo usuario.
(define S18 ((run S17 login) "admin"))
(define S19 ((run S18 login) "admin")) ;como "admin" ya inició sesión, no se modifica.

;cambios de unidad, incluyendo unidad inexistente K
(define S20 ((run S19 switch-drive) #\K))
(define S21 ((run S20 switch-drive) #\C))

;se cierra la sesión y no puede cambiar de drive porque no existe una sesión iniciada
(define S22 (run S21 logout))
(define S23 ((run S22 switch-drive) #\e))

#|inicia sesión y cambia el drive, se ingresa una letra en mayúscula a pesar de que el sistema
guarda los drives en minúscula, esto es posible ya que los drives no son case-sensitive.|#
(define S24 ((run S23 login) "guest"))
(define S25 ((run S24 switch-drive) #\E))
(define S26 ((run S25 switch-drive) #\c))

;añadiendo carpetas. Incluye casos de carpetas duplicadas.
(define S27 ((run S26 md) "folder1"))
(define S28 ((run S27 md) "folder2"))
(define S29 ((run S28 md) "folder2"))
(define S30 ((run S29 md) "folder3"))

#|se trata de ingresar una carpeta con un nombre con mayúsculas, lo cual no funciona ya que
el nombre de esa carpeta en esa ruta ya existe en minúsculas. Por lo que se cambia el drive
y se crea la carpeta.|#
(define S31 ((run S30 md) "FOLDER3"))
(define S32 ((run S31 switch-drive) #\F))
(define S33 ((run S32 md) "FOLDER3"))

;se cambia la ruta actual al root del drive C
(define S34 ((run S33 cd) "C:/"))

;ingresa a carpeta folder2
(define S35 ((run S34 cd) "folder2"))

;crea subcarpeta folder21 dentro de folder2 (incluye caso S19 de carpeta con nombre duplicado)
(define S36 ((run S35 md) "folder21"))
(define S37 ((run S36 md) "folder21"))

;se crea la carpeta folder22 como subcarpeta de folder2
(define S38 ((run S37 md) "folder22"))

;ingresa a subcarpeta e intenta ingresar a subcarpeta inexistente S21
(define S39 ((run S38 cd) "folder21"))
(define S40 ((run S39 cd) "folder22"))

;vuelve a carpeta anterior
(define S41 ((run S40 cd) ".."))

;se mueve directamente a la carpeta folder1 y crea la carpeta folder1
(define S42 ((run S41 cd) "c:/folder1/"))
(define S43 ((run S42 md) "folder1")) #|Es posible crear carpetas con el mismo nombre como
una subcarpeta|#

;vuelve a la carpeta anterior y entra a folder2
(define S44 ((run S43 cd) ".."))
(define S45 ((run S44 cd) "folder2"))

;vuelve a ingresar folder21
(define S46 ((run S45 cd) "folder21"))

;crea subcarpeta folder211 e ingresa
(define S47 ((run S46 md) "folder211"))
(define S48 ((run S47 cd) "folder211"))

;vuelve a la raíz de la unidad c:/
(define S49 ((run S48 cd) "/"))

;se cambia de unidad
(define S50 ((run S49 switch-drive) #\D))

;crea carpeta e ingresa a carpeta
(define S51 ((run S50 md) "folder5"))
(define S52 ((run S51 cd) "folder5"))

;se cambia de carpeta en base a la ruta especificada
(define S53 ((run S52 cd) "C:/folder1/"))

;añadiendo archivos
(define S54 ((run S53 add-file) (file "foo1.txt" "txt" "hello world 1")))
(define S55 ((run S54 add-file) (file "foo2.txt" "txt" "hello world 2")))
(define S56 ((run S55 add-file) (file "foo3.docx" "docx" "hello world 3")))
(define S57 ((run S56 add-file) (file "goo4.docx" "docx" "hello world 4" #\h #\r))) ;con atributos de seguridad oculto (h) y de solo lectura (r)

;se intenta añadir un archivo con un nombre que ya fue creado
(define S58 ((run S57 add-file) (file "foo3.docx" "docx" "hello world 5" #\r)))

;se intenta agregar un archivo con el mismo nombre pero en mayúsculas
(define S59 ((run S57 add-file) (file "FOO3.DOCX" "docx" "hello world 3")))

;eliminando archivos
(define S60 ((run S57 del) "goo4.docx"))
(define S61 ((run S57 cd) ".."))
(define S62 ((run S61 del) "folder1")) ;elimina folder1 y su subcarpeta

;se ingresa al subdirectorio folder2/folder22 y se crean 2 archivos
(define S63 ((run S62 cd) "folder2/folder22"))
(define S64 ((run S63 add-file) (file "foo4.docx" "docx" "hello world 6" #\h)))
(define S65 ((run S64 add-file) (file "prueba.txt" "txt" "contenidos variados")))

;se elemina el archivo "foo4.docx" y después la carpeta folder2 y sus subcarpetas
(define S66 ((run S65 del) "Foo4.docx"))
(define S67 ((run S66 cd) "/"))
(define S68 ((run S67 del) "FOLDER2"))

;borrando una carpeta
(define S69 ((run S61 rd) "folder1"))  ;no debería borrarla, pues tiene archivos
(define S70 ((run S69 cd) "folder1"))
(define S71 ((run S70 del) "foo1.txt"))
(define S72 ((run S71 del) "foo2.txt"))
(define S73 ((run S72 del) "foo3.docx"))
(define S74 ((run S73 del) "goo4.docx"))
(define S75 ((run S74 del) "goo4.docx")) ;trata de borrar un archivo que ya no existe

;se devuelve y trata de borrar folder1, pero no es posible ya que tiene una subcarpeta
(define S76 ((run S75 cd) ".."))
(define S77 ((run S76 rd) "folder1"))

;desde afurea borra la subcarpeta de igual nombre, para después salir y borrar folder1
(define S78 ((run S77 rd) "folder1/folder1"))
(define S79 ((run S78 rd) "folder1"))

;copiando carpetas y archivos
(define S80 ((run S57 copy) "foo1.txt" "c:/folder3/"))
(define S81 ((run S80 cd) ".."))
(define S82 ((run S81 copy) "folder1" "d:/"))
(define S83 ((run S82 copy) "folder2" "e:/"))

;intenta copiar una carpeta que ya existe en la ruta objetivo
(define S84 ((run S83 copy) "folder2" "e:/")) ;no se logra copiar ya que folder2 ya existe en la ruta objetivo

;intenta copiar una carpeta que no existe
(define S85 ((run S84 copy) "folder4" "f:/"))

;intenta copiar una carpeta a una dirección que no existe
(define S86 ((run S85 copy) "folder3" "a:/folder1/"))

;moviendo carpetas y archivos
(define S87 ((run S86 move) "folder3" "d:/"))
(define S88 ((run S87 cd) "folder1"))
(define S89 ((run S88 move) "foo3.docx" "d:/folder3/"))

;intenta mover una carpeta que no existe
(define S90 ((run S89 move) "folder8" "c:/folder2/folder21/"))

;intenta mover un archivo a una ruta que no existe
(define S91 ((run S90 move) "foo1.txt" "f:/folder5/"))

;intenta mover un archivo a una ruta en que ya existe un archivo con el mismo nombre
(define S92 ((run S91 move) "foo1.txt" "d:/folder3"))

;renombrando carpetas y archivos
(define S93 ((run S92 ren) "foo1.txt" "newFoo1.txt"))
(define S94 ((run S93 ren) "foo2.txt" "newFoo1.txt")) ;no debería efectuar cambios pues ya existe archivo con este nombre
(define S95 ((run S94 cd) ".."))
(define S96 ((run S95 ren) "folder1" "newFolder1"))

;cambia el nombre de folder2 con mayúsculas
(define S97 ((run S96 ren) "folder2" "newFOLDER2"))

;intenta renombrar una carpeta al de una que ya existe
(define S98 ((run S97 ren) "newfolder2" "newfolder1"))

;intenta cambiar el nombre de un archivo que no existe
(define S99 ((run S98 cd) "d:/folder3/"))
(define S100 ((run S99 ren) "foo2.txt" "foo4.docx"))

;cambia el nombre y tipo de archivo
(define S101 ((run S100 ren) "foo3.docx" "foo3.txt"))