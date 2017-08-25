#lang plai 

#| Práctica 1: Introducción a Racket |#

;; Función que toma dos números enteros positivos y eleva uno al otro, para luego sumar las raíces
;; cuadradas de éstos.
;; rps: number number -> number
(define (rps a b)
    (+ (sqrt (expt a b)) (sqrt (expt b a)))) 

;; Función que encuentra el área de un tirángulo dados sus lados, usando la fórmula de Herón. Se usa
;; la primitiva let para evitar cálculos repetitivos.
;; area-heron: number number number -> number
(define (area-heron a b c)
    (let ([s (/ (+ a b c) 2)])
        (sqrt (* s (- s a) (- s b) (- s c)))))

;; Predicado que determina si la pareja a b entará en el antro usando condicionales. La respuesta de 
;; el predicado está dada de acuerdo a lo siguiente:
;; "Si el estilo de los asistentes es de ocho o más, el predicado responderá 'si con la excepción de
;;  que si el estilo de alguno de los asistentes es de dos o menos, responderá 'no. En otro caso,
;;  responderá 'quiza."
;; entra?: number number -> symbol
(define (entra? a b)
    (cond
        [(and (< a 8) (and (> b 2) (< b 8))) 'quiza]
        [(or (and (> a 7) (> b 2)) (and (> a 2) (> b 7))) 'si]
        [(or (< a 3) (< b 3)) 'no]))   

;; Función recursiva que regresa el número de apariciones del dígito m como digito en el número 
;; entero positivo n.
;; apariciones: number number -> number
(define (apariciones n m)
   (cond
       [(< n 10 ) (if (eq? n m) 1 0)]
       [(eq? (modulo n 10) m) (+ 1 (apariciones (remainder n 10) m))]
       [(apariciones (remainder n 10) m)]))

;; Función recursiva que calcula el número de pares de una cadena. Decimos que un par en una cadena
;; son dos caracteres idénticos, separados por un tercero. Por ejemplo "AxA" es el par de "A". Los
;; pares, además, pueden anidarse, por ejemplo.
;; cuenta-pares: string -> number
(define (cuenta-pares c)
    (let ([lst (string->list c)])
        (cond
            [(eq? '() lst) 0]
            [(and (> (length lst) 2) (eq? (first lst) (third lst)))
                (+ 1 (cuenta-pares (list->string (rest lst))))]
            [else (cuenta-pares (list->string (rest lst)))])))

;; Función que imprime una piramide con n pisos haciendo uso de alguna función de impresión.
;; piramide: number -> void
(define (piramide n)
  (letrec (
      [print-renglon (lambda (n c)
          (display (string-append (make-string n #\ ) (make-string c #\*) "\n"))
          (if (eq? 0 n) (display "") (print-renglon (- n 1) (+ c 2))))])
       (if (> n 0) (print-renglon (- n 1) 1) (display ""))))

;; Función que recibe dos listas y construye una nueva lista con listas de longitud 2 formadas a 
;; partir de los elementos de ambas listas.
;; arma-pares: list list -> (listof list)
(define (arma-pares lst1 lst2)
    (match lst1
        ['() (match lst2
            ['() '()]
            [(cons x xs) '()])]
        [(cons x xs) (match lst2
            ['() '()]
            [(cons y ys) (cons (cons x (cons y '())) (arma-pares xs ys))])]))

;; Función que recibe una lista con elementos de la forma '(id value) y regresa el valor asociado al
;; id que fue pasado como parámetro.
;; lookup: (listof list) -> any
(define (lookup id lst)
    (match lst
        ['() '()]
        [(cons x xs) (cond
            [(eq? (car x) id) (second x)]
            [else (lookup id xs)])]))

;; Función que compara la longitud de las listas lst1 y lst2. El valor de regreso son alguno de los 
;; siguientes:
;; · 'lista1-mas-grande
;; · 'lista2-mas-grande
;; · 'listas-iguales
;; compara-longitud: list list -> symbol
(define (compara-longitud lst1 lst2)
    (letrec ( 
        [longitud (lambda (lst)
            (if (eq? '() lst) 0 (+ 1 (longitud (cdr lst)))))])
        (cond
            [(> (longitud lst1) (longitud lst2)) 'lista1-mas-grande]
            [(< (longitud lst1) (longitud lst2)) 'lista2-mas-grande]
            ['listas-iguales])))

;; Función que entierra el símbolo nombre, n número de veces. Es decir, se anidan n - 1 listas hasta
;; que se llega a la lista que tiene al símbolo nombre.
;; entierra: symbol number -> list
(define (entierra nombre n)
    (if (eq? n 0) nombre (cons (entierra nombre (- n 1)) '())))

;; Función que que mezcla dos listas ordenadas obtieniendo una nueva, ordenada de manera ascendente.
;; mezcla: list list -> list
(define (mezcla lst1 lst2)
    (match lst1
        ['() (match lst2
            ['() '()]
            [(cons x xs) lst2])]
        [(cons x xs) (match lst2
            ['() lst1]
            [(cons y ys)
                (cond
                    [(<= x y) (cons x (mezcla xs (cons y ys)))]
                    [else (cons y (mezcla (cons x xs) ys))])])]))

;; Función que recibe una lista de números y regresa una nueva lista de cadenas que representan al
;; número binario asociado a estos números.
;; binarios: (listof number) -> (listof string)
(define (binarios lst)
    (letrec (
        [int->str-binary (lambda (n)
            (cond
                [(zero? n) "0"]
                [(eq? n 1) "01"]
                [else (string-append (int->str-binary (quotient n 2)) (number->string (remainder n 2)))]))])
     (map int->str-binary lst)))

;; Función que recibe una lista y regresa una nueva conteniendo únicamente aquellos que son 
;; triangulares.
;; triangulares: (listof number) -> (listof number)
(define (triangulares lst)
    (letrec (
        [triangular? (lambda (n)
            (integer? (sqrt (+ (* 8 n) 1))))])
        (filter triangular? lst)))

;; Función que, usando foldr, intercala un símbolo dado entre los elementos de una lista.
;; intercalar: list symbol -> list
(define (intercalar lst s)
    (match lst
        ['() '()]
        [(cons x xs) (cons x (foldr (lambda (y ys) (list* s y ys)) '() xs))]))

;; Función que, usando foldl, intercala un símbolo dado entre los elementos de una lista.
;; intercalal: list symbol -> list
(define (intercalal lst s)
    (letrec (     
        [reversa (lambda (lst)
            (cond
                [(eq? '() lst) '()]
                [else (cons (reversa (cdr lst)) (list (car lst)))]))])
        (match lst
            ['() '()]
            [(cons x xs) (cons x (reverse (foldl (lambda (y ys) (list* y s ys)) '() xs)))])))



