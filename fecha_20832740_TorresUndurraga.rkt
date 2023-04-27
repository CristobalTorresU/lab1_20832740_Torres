#lang racket
(provide fecha)
;Implementación del TDA fecha

#|REPRESENTACIÓN: Este TDA representa una fecha.
    Una lista de 2 listas, la primera tiene las horas, minutos y
segundos. La segunda lista tiene el día, mes y año.|#

;CONSTRUCTOR
;descripción: Función que entrega la hora y fecha.
;dom: nada
;rec: fecha
(define fecha (lambda ()
                (list (list (date-hour (seconds->date (current-seconds)))
                            (date-minute (seconds->date (current-seconds)))
                            (date-second (seconds->date (current-seconds))))
                      (list (date-day (seconds->date (current-seconds)))
                            (date-month (seconds->date (current-seconds)))
                            (date-year (seconds->date (current-seconds)))))))
