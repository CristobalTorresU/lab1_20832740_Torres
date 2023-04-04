#lang racket
(provide fecha)
;Implementación del TDA fecha

;CONSTRUCTOR
;descripción: Función que entrega la hora y fecha.
;dom: segundos (int)
;rec: fecha
(define fecha (lambda (segundos)
                (list (list "Hora" (date-hour (seconds->date (current-seconds)))
                      (date-minute (seconds->date (current-seconds)))
                      (date-second (seconds->date (current-seconds))))
                      (list "Fecha" (date-day (seconds->date (current-seconds)))
                      (date-month (seconds->date (current-seconds)))
                      (date-year (seconds->date (current-seconds)))))))