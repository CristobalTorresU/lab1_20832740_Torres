#lang racket
(require "fecha.rkt")
(provide (all-defined-out))

;Implementación del TDA system

#|REPRESENTACIÓN: |#

;CONSTRUCTOR
(define sistema (lambda (name) (list (list name (fecha) "N/A" "N/A" "N/A") null null null)))

;SELECTORES

(define datos_sistema (lambda (system) (list-ref system 0)))

(define unidades (lambda (system) (list-ref system 1)))

(define usuarios (lambda (system) (list-ref system 2)))

(define papelera (lambda (system) (list-ref system 3)))