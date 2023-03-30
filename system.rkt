#lang racket
;Implementación del TDA system

;CONSTRUCTOR
;descripción: Permite crear un sistema
;dom: string
;rec: lista
(define system (lambda (name) (list name (seconds->date (current-seconds)))))