#lang plai

#| Práctica 1: Introducción a Racket |#

;; Función que toma dos números enteros positivos y eleva uno al otro, para luego sumar las raíces
;; cuadradas de éstos.
;; rps: number number -> number
(define (rps a b)
 (+ (sqrt (rpsaux a b)) (sqrt (rpsaux b a)))) 
  
;; Función que toma dos números enteros positivos y eleva uno al otro
;; rpsaux: number number -> number
(define (rpsaux a b)
     (cond[(= b 0) 1]
        [else (* a (rpsaux a (sub1 b)))]))

;; Función que encuentra el área de un tirángulo dados sus lados, usando la fórmula de Herón. Se usa
;; la primitiva let para evitar cálculos repetitivos.
;; area-heron: number number number -> number
(define (area-heron a b c)
  (let([s (/ (+ a b c) 2)])
   (sqrt(* s (- s a)(- s b)(- s c)))))

;; Predicado que determina si la pareja a b entará en el antro usando condicionales. La respuesta de 
;; el predicado está dada de acuerdo a lo siguiente:
;; "Si el estilo de los asistentes es de ocho o más, el predicado responderá 'si con la excepción de
;;  que si el estilo de alguno de los asistentes es de dos o menos, responderá 'no. En otro caso,
;;  responderá 'quiza."
;; entra?: number number -> symbol
(define (entra? a b)
  (cond[(and (< a 8)(and(> b 2)(< b 8))) (display "'quiza")]
  [(or(and (> a 7)(> b 2)) (and (> a 2) (> b 7))) (display "'si")]
  [(or(< a 3) (< b 3)) (display "'no")]))   

;; Función recursiva que regresa el número de apariciones del dígito m como digito en el número 
;; entero positivo n.
;; apariciones: number number -> number
(define (apariciones n m)
   (cond
     [(< n 10 ) (if (= n m) 1 0)]
     [(= (modulo n 10) m) (+ 1 (apariciones (quotient n 10) m))]
     [(apariciones (quotient n 10) m)]))

;; Función recursiva que calcula el número de pares de una cadena. Decimos que un par en una cadena
;; son dos caracteres idénticos, separados por un tercero. Por ejemplo "AxA" es el par de "A". Los
;; pares, además, pueden anidarse, por ejemplo.
;; cuenta-pares: string -> number
(define (cuenta-pares c)
  ( letrec (
      [tiene-par( λ (cadena)
        (if (equal? (string-ref cadena 0)(string-ref cadena 2)) #t #f))])                   
  (if(<(string-length c)3)
      0
     (if(tiene-par c)
        (add1 (cuenta-pares(substring c 1  (string-length c))))
     (cuenta-pares(substring c 1  (string-length c) ))))))
  

;; Función que imprime una piramide con n pisos haciendo uso de alguna función de impresión.
;; piramide: number -> void
(define (piramide n)
  (if (> n 0) (piramide-aux (- n 1) 1) (display "")))
          
(define (piramide-aux n c)
  (display (string-append (make-string n #\ ) (make-string c #\*) "\n"))
  (if (eq? 0 n) (display "") (piramide-aux (- n 1) (+ c 2))))

;; Función que recibe dos listas y construye una nueva lista con listas de longitud 2 formadas a 
;; partir de los elementos de ambas listas.
;; arma-pares: list list -> (listof list)
(define (arma-pares lst1 lst2)
   (match lst1
     ['() (match lst2
          [empty empty]
          [(cons x xs) '()])]
     [(cons x xs) (match lst2
          [empty empty]
          [(cons y ys) (cons (cons x (cons y empty)) (arma-pares xs ys))])]))

;; Función que recibe una lista con elementos de la forma '(id value) y regresa el valor asociado al
;; id que fue pasado como parámetro.
;; lookup: (listof list) -> any
(define (lookup id lst)
   (match lst
     ['() null]
     [(cons x xs) (if (eq? (car x) id) (second x) (lookup id xs))]))

;; Función que compara la longitud de las listas lst1 y lst2. El valor de regreso son alguno de los 
;; siguientes:
;; · 'lista1-mas-grande
;; · 'lista2-mas-grande
;; · 'listas-iguales
;; compara-longitud: list list -> symbol
(define (compara-longitud lst1 lst2)
   ( letrec (
      [ longitud ( λ ( lst )
          (if (null? lst)
              0
              (+ 1 (longitud (cdr lst)))))])      
   (cond
    [(< (longitud lst1) (longitud lst2)) 'lista2-mas-grande]
    [(> (longitud lst1) (longitud lst2)) 'lista1-mas-grande]
    ['listas-iguales])))   
    

;; Función que entierra el símbolo nombre, n número de veces. Es decir, se anidan n - 1 listas hasta
;; que se llega a la lista que tiene al símbolo nombre.
;; entierra: symbol number -> list
(define (entierra nombre n)
   (cond
     [(eq? n 0) nombre ]
     [else (cons (entierra nombre (- n 1)) empty)]))

;; Función que que mezcla dos listas ordenadas obtieniendo una nueva, ordenada de manera ascendente.
;; mezcla: list list -> list
(define (mezcla lst1 lst2)
    (cond 
     [(empty? lst1) lst2]
     [(empty? lst2) lst1]
     [(<= (car lst1) (car lst2))
      (cons (car lst1) (mezcla (cdr lst1) lst2))]
     [(cons (car lst2) (mezcla lst1 (cdr lst2)))]))

;; Función que recibe una lista de números y regresa una nueva lista de cadenas que representan al
;; número binario asociado a estos números.
;; binarios: (listof number) -> (listof string)
(define (binarios lst)
    ( letrec (
      [ conversion ( λ ( n )
        (cond [(= n 0) (number->string n)]
        [(= n 1) (string-append "0" (number->string n))]
        [else  (string-append (conversion (quotient n 2)) (number->string (remainder n 2)))]))])
  (map conversion lst)))

;; Función que recibe una lista y regresa una nueva conteniendo únicamente aquellos que son 
;; triangulares.
;; triangulares: (listof number) -> (listof number)
(define (triangulares lst)
  (letrec (
     [triang? (λ (n)
            (integer? (sqrt (+ (* 8 n)  1))))])
  (cond
    [(empty? lst) '()]
    [(triang? (car lst)) (cons (car lst) (triangulares (cdr lst)))]
    [(triangulares (cdr lst))])))

;; Función que, usando foldr, intercala un símbolo dado entre los elementos de una lista.
;; intercalar: list symbol -> list
(define (intercalar lst s)
  (match lst
    ['() '()]
    [(cons a bc) (cons a (foldr (lambda (x z) (list* s x z)) empty bc))]))
;;Función que definimos para poder concatenar dos listas 
;;concatena: list -> list
(define (concatena lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (concatena (cdr lst1) lst2))))

;;Función que definimos para que regrese la reversa de una lista
;;reversa: list ->list
(define (reversa lst1)
  (cond
    ((null? lst1) '())
    ((list? lst1) (concatena (reversa(cdr lst1))(list(car lst1))))
    (else (error '(No hay lista)))))

;; Función que, usando foldl, intercala un símbolo dado entre los elementos de una lista.
;; intercalal: list symbol -> list
(define (intercalal lst s)
   (cond
     [(eq? '() lst) '()]
     [(letrec (
               [mete-asterisco (lambda (x v)
                                 (cons x (cons s v)))]
               [quita-asterisco (lambda (lista)
                                  (reversa (cdr (reversa lista))))])
       (reversa (quita-asterisco (foldr mete-asterisco '() lst))))]))