#lang racket

;; Defina una función para la evaluación del número combinatorio C(n,k), que utiliza la definición recursiva. 
(define (evalCombinatoria n k)
  (if (or (= k 0) (= k n))
      1
      (if (and (>= k 0) (>= n 0) (>= n k))
          (+ (evalCombinatoria (- n 1) (- k 1))
             (evalCombinatoria (- n 1) k))
          (displayln "Error de Sintaxis"))))

;;Defina una función recursiva para calcular el Máximo Común Divisor de dos enteros negativos a y b con a < b usando el hecho de que MCD(a, b) = MCD(a, b-a).
(define (MCD a b)
  (if (and (< a 0) (< b 0) (< a b))   
      (mcd_pos (abs a) (abs b))
      "Error de Sintaxis"))
      
(define (mcd_pos a b)
  (cond ((= a b) (abs a))                        
        ((< a b) (mcd_pos a (- b a)))                
        (else (mcd_pos b (- a b)))))   


;;Definir una función que devuelva, en una lista, todos los números primos desde un númer inicial hasta un número final, ejemplo: (primos 3 10) este ejemplo devolverá ‘(5 7).   

 (define (es_primo n)
  (cond ((<= n 1) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else
         (let loop ((i 3))
           (cond ((> (* i i) n) #t)
                 ((zero? (modulo n i)) #f)
                 (else (loop (+ i 2))))))))

(define (primos inicio fin)
  (let loop ((n inicio) (result '()))
    (if (> n fin)
        (reverse result)
        (if (es_primo n)
            (loop (+ n 1) (cons n result))
            (loop (+ n 1) result)))))

;;Realizar una función para buscar un elemento en una lista, regresar #t si lo encontró y #f sino lo encontró
(define (busca x lista)
  (cond ((null? lista) #f)  
        ((equal? x (car lista)) #t)  
        (else (busca x (cdr lista)))))

;; Realizar una función recursiva que invierta una lista
(define (invierte lista)
  (define (invierte-aux lista acumulado)
    (if (null? lista)
        acumulado
        (invierte-aux (cdr lista) (cons (car lista) acumulado))))
  (invierte-aux lista '()))

;;Realizar una función recursiva que elimine un elemento de una lista
(define (elimina elemento lista)
  (cond ((null? lista) '())  
        ((equal? (car lista) elemento) (elimina elemento (cdr lista)))  
        (else (cons (car lista) (elimina elemento (cdr lista)))))) 
;;Dado un número entero positivo, realizar una función recursiva que devuelva verdadero (#t) si el número dado es un palíndromo, en caso contrario, retornar falso (#f).

(define (numero-a-lista n)
  (if (< n 10)
      (list n)
      (append (numero-a-lista (quotient n 10))
              (list (remainder n 10)))))

(define (sin_primer_y_ultimo lst)
  (if (or (null? lst) (null? (cdr lst)))
      '() 
      (cdr (reverse (cdr (reverse lst))))))

(define (es_palindromo lst)
  (cond ((or (null? lst) (null? (cdr lst))) #t)  
        ((not (= (car lst) (last lst))) #f)
        (else (es_palindromo (sin_primer_y_ultimo lst)))))

(define (palindromo n)
  (es_palindromo (numero-a-lista n)))
;;Realizar una función recursiva que, dado un número entero, encuentra la suma de susdígitos. No use funciones incorporadas de Racket.

(define (SumaDigitos numero)
  (define lista (numero-a-lista numero)) ;;Numero a lista usado en la anterior
  (define (sumar-lista lst)
    (if (null? lst)
        0
        (+ (car lst) (sumar-lista (cdr lst))))) 
  (sumar-lista lista))

;;Realizar una función recursiva que, dado un número entero decimal retorne el número binario equivalente. No use funciones incorporadas de Racket.
(define (binario n)
  (if (= n 0)
      '()
      (append (binario (quotient n 2))
              (list (remainder n 2)))))

;;Utilizando la serie de Leibnitz y mediante una función recursiva, calcule el valor de PI

(define (PI n)
  (define (leibniz-serie termino)
    (/ (exponente-uno termino) (exponente-dos termino)))
  
  (define (exponente-uno termino)
    (expt -1 termino))
  
  (define (exponente-dos termino)
    (+ (* 2 termino) 1))
  
  (define (iterar-terminos termino contador acumulador)
    (if (<= contador 0)
        acumulador
        (iterar-terminos (+ termino 1)
                         (- contador 1)
                         (+ acumulador (leibniz-serie termino)))))
  
  (* 4 (iterar-terminos 0 n 0)))
