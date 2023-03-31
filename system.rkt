#lang racket
;Implementación del TDA system

;CONSTRUCTOR
;descripción: Permite crear un sistema y registra la fecha de creación
;dom: string
;rec: lista
(define system (lambda (name) (list name (seconds->date (current-seconds)))))

;MODIFICADOR
;descripción: 
;dom: system x command (función)
;rec: system
(define run (lambda (system command) (command system)))