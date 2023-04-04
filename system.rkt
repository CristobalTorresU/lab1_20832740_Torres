#lang racket
(require "drive.rkt" "user.rkt" "fecha.rkt")

;Implementación del TDA system

;CONSTRUCTOR
;descripción: Permite crear un sistema y registra la fecha de creación.
;dom: string
;rec: lista
(define system (lambda (name) (list (list name (fecha (current-seconds))) (list "N/A") (list "N/A") (list) (list))))

;MODIFICADOR
;descripción: Función que permite ejecutar un comando sobre un sistema.
;dom: system x command (función)
;rec: system
(define run (lambda (system command) (command system)))