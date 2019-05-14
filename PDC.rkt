#lang racket

;; Tarea Progamada Problema del Caballo
;; Prof: Marco Vinicio
;; Alumnos: Dario Rodríguez
;;          Jonathan Guzmán
;;          Kenneth Hernández

(require "Interfaz.rkt") ;; Si se desea utilizar la interfaz se debe descomentar esto.

;Esta funcion lo que hace es crear una matriz, la primera llamada n1 y n2 son iguales y la lista es una vacia
(define (crear_matriz n1 n2 lista)
  (cond(
        (equal? n2 0)
        lista)
       (else (crear_matriz n1  (- n2 1) (append lista (list (crear_lista_aux n1 '())))))))
;Crea una lista de n elementos, la primera llamada recibe n y una lista vacia
(define ( crear_lista_aux n lista)
  (cond (
         (equal? n 0)
         lista)
        (else (crear_lista_aux(- n 1)(cons 0 lista)))))
;saca el largo a una matriz o lista
(define ( largo matriz)
  (cond (
         (null? matriz)
         0)
        (else (+ 1 (largo (cdr matriz) ))))) 
  
;esta funcion retorna un falso o verdadero, dependiendo de si el movimiento que se planea hacer es valido
(define ( can_move n1 n2 matriz)
  (cond (
         (equal? n1 0)
         (can_move_aux n2 (car  matriz)))
        (else
         (can_move (- n1 1) n2 (cdr matriz))
        )))
;esta es la auxiliar, en esta se mueve en la lista que se espera conseguir de la no auxiliar, retorna T si se puede
;hacer el movimiento,sino retorna f
(define (can_move_aux n lista)
  (cond 
         (
         (null? lista)
         #f)
         ( (and ( equal? n 0) (equal? (car lista) 0)) 
         #t)
        
        (else (can_move_aux (- n 1) (cdr lista)))))
              
;Esta funcion hace el movimiento en la matriz,las primeras iteraciones al menos  
(define ( movimiento n1 n2 matriz numero largo)
  ( cond (
          (or (> n1 (- largo 1)) (> n2 (- largo 1)))
          '() )
         (
          (or (< n1 0) (< n2 0))
          '() )
         (
           (can_move n1 n2 matriz)
          (movimiento_aux n1 n2 matriz '() largo numero ))
         (else  '() )))
(define (movimiento_aux n1 n2 matriz lista largo numero)
  (cond (
         (equal? largo 0)
         lista)
        (  
         (equal? n1 0)  
        ( movimiento_aux (- n1 1) n2 (cdr matriz) (append lista (list(movimiento_lista n2 numero (car matriz) '()))) (- largo 1) numero ))
        (else ( movimiento_aux (- n1 1) n2 (cdr matriz) (append lista (list (car matriz))) (- largo 1) numero))
        )
  )

(define ( movimiento_lista n numero lista resultado) 
  (cond (
         (null? lista)
        resultado)
        (
         (equal? n 0)
         (movimiento_lista (- n 1) numero (cdr lista) (append resultado (list numero )) )
         )
        (else (movimiento_lista (- n 1) numero (cdr lista) (append resultado ( list (car lista))))
              )
        )
  ) 

(define (PDC-Todas n coordenada)
  (resolver_todas  (movimiento  (car coordenada) (car (cdr coordenada)) (crear_matriz n n '()) 1 n) 1 (car coordenada) (car (cdr coordenada)) n ))

(define (resuelta? matriz )
  (cond(
        (null? matriz)
        #t)
       (
        (not (contiene_0? (car matriz)))
        #f)
       (else (resuelta? (cdr matriz)))))
(define (contiene_0? lista)
  (cond(
        (null? lista)
        #t)
       (
        (equal? 0 (car lista))
        #f)
       (else (contiene_0? (cdr lista)))))

(define (resolver_todas matriz num_movimiento mov_x mov_y largo)
  (cond(
        (null? matriz)
        
        '())
        (
         (equal?   (* largo largo)  num_movimiento)
        (list matriz))
        (else (append (append (append (append (append ( append ( append (append
                       '()
                      (resolver_todas  (movimiento  (+ 2 mov_x) (+ 1 mov_y)  matriz (+ 1 num_movimiento) largo) (+ 1 num_movimiento) (+ 2 mov_x) (+ 1 mov_y) largo ))
                      (resolver_todas  (movimiento  (+ 1 mov_x) (+ 2 mov_y)  matriz (+ 1 num_movimiento) largo) (+ 1 num_movimiento) (+ 1 mov_x) (+ 2 mov_y) largo ))
                      (resolver_todas  (movimiento  (+ 2 mov_x) (- mov_y 1)  matriz (+ 1 num_movimiento) largo) (+ 1 num_movimiento) (+ 2 mov_x) (- mov_y 1) largo ) )
                      (resolver_todas  (movimiento  (+ 1 mov_x) (- mov_y 2)  matriz (+ 1 num_movimiento ) largo) (+ 1 num_movimiento) (+ 1 mov_x) (- mov_y 2) largo ))
                      (resolver_todas  (movimiento  (- mov_x 1) (+ mov_y 2)  matriz (+ 1 num_movimiento) largo) (+ 1 num_movimiento) (- mov_x 1) (+ mov_y 2) largo ))
                      (resolver_todas  (movimiento  (- mov_x 2) (+ mov_y 1)  matriz (+ 1 num_movimiento) largo) (+ 1 num_movimiento) (- mov_x 2) (+ mov_y 1) largo ))
                      (resolver_todas  (movimiento  (- mov_x 1) (- mov_y 2)  matriz (+ 1 num_movimiento) largo) (+ 1 num_movimiento) (- mov_x 1) (- mov_y 2) largo))
                      (resolver_todas  (movimiento  (- mov_x 2) (- mov_y 1)  matriz (+ 1 num_movimiento) largo) (+ 1 num_movimiento) (- mov_x 2) (- mov_y 1) largo )))))


(define ( search_value inicio valor matriz)
  (cond (
         (null? matriz)
         '()
         )
        (
         (< -1 (search_value_lista (car matriz) valor 0) )
         (append (list inicio) (list (search_value_lista (car matriz) valor 0)))
         )
        (else (search_value (+ inicio 1) valor (cdr matriz)
                            )
              )
        )
  )
(define (search_value_lista lista valor cuenta)
  (cond
    (
     (null? lista)
     -2)
    (
     (equal? valor (car lista))
     cuenta)
    (else (search_value_lista (cdr lista) valor (+ cuenta 1)))))


(define (PDC-Test n sol_matriz )
  ( comprueba_sol (crear_matriz n n '()) sol_matriz (search_value 0 1 sol_matriz) (search_value 0 2 sol_matriz) 1 n))



(define (comprueba_sol matriz solucion primer_coordenada segunda_coordenada elemento_actual largo )
  (cond
    (
     (null? segunda_coordenada)
    (movimiento  (car primer_coordenada)  (car (cdr primer_coordenada))  matriz elemento_actual largo))
       (
        (null? primer_coordenada)
        matriz
        )
       (
        (valido? primer_coordenada segunda_coordenada)
        (comprueba_sol (movimiento  (car primer_coordenada)  (car (cdr primer_coordenada))  matriz elemento_actual largo)
                       solucion segunda_coordenada (search_value  0 (+ elemento_actual 2) solucion) (+ elemento_actual 1)
                       largo ))
       (else (movimiento  (car primer_coordenada)  (car (cdr primer_coordenada))  matriz elemento_actual largo)
        )))

; Aqui lo que hace es ver si las cooordenadas son validas para un elemento determinado, siendo coor1 las iniciales y coor2 las finales

(define (valido? coor1 coor2)
  (cond
    (
     (and (equal? (+ (car coor1) 2) (car coor2)) (equal? (+ (car(cdr coor1)) 1) (car (cdr coor2))))
     #t)
    (
     (and (equal? (+ (car coor1) 1) (car coor2)) (equal? (+(car(cdr coor1)) 2) (car (cdr coor2))))
     #t)
    (
     (and (equal? (+ (car coor1) 2) (car coor2)) (equal? (-(car(cdr coor1)) 1) (car (cdr coor2))))
     #t)
    (
     (and (equal? (+ (car coor1) 1) (car coor2)) (equal? (-(car(cdr coor1)) 2) (car (cdr coor2))))
     #t)
    (
     (and (equal? (- (car coor1) 1) (car coor2)) (equal? (+(car(cdr coor1)) 2) (car (cdr coor2))))
     #t)
    (
     (and (equal? (- (car coor1) 2) (car coor2)) (equal? (+(car(cdr coor1)) 1) (car (cdr coor2))))
     #t)
    (
     (and (equal? (- (car coor1) 1) (car coor2)) (equal? (- (car(cdr coor1)) 2) (car (cdr coor2))))
     #t)
    (
     (and (equal? (- (car coor1) 2) (car coor2)) (equal? (-(car(cdr coor1)) 1) (car (cdr coor2))))
     #t)
    (else #f)))


;(define (PDC-Sol n posicion)
;  ( crear_matriz_pasos (movimiento  (car posicion) (car (cdr posicion)) (crear_matriz n n '()) 1) 1 (car posicion) (car (cdr posicion) )  '() n))
; Recibe una posicion y calcula la cantidad de posiciones disponibles, si no es una posicion
; 



(define ( calc_posiciones  fila columna   matriz iteracion largo)
  (cond
    (
     (or (> 0 fila) (> 0 columna))
     99)
    (
     (equal? iteracion 0) 
     0)
    (
     (not (can_move2 fila columna matriz largo) )
     99)
    (else
     (+ (puede_mover matriz fila columna   iteracion largo)  (calc_posiciones fila columna  matriz (- iteracion 1) largo)))))

    
     
  
(define (puede_mover matriz fila columna iteracion largo)
  (cond
    (
    (equal? iteracion 8)
    (cond
      (
       ( can_move2 (- fila 1) (+ columna 2) matriz largo)
       1)
      (else 0)))
    (
    (equal? iteracion 7)
    (cond
      (
       ( can_move2 (- fila 2) (+ columna 1) matriz largo)
       1)
      (else 0)))
    (
    (equal? iteracion 6)
    (cond
      (
       ( can_move2 (- fila 2) (- columna 1) matriz largo)
       1)
      (else 0)))
    (
    (equal? iteracion 5)
    (cond
      (
       ( can_move2 (- fila 1) (- columna 2) matriz largo)
       1)
      (else 0)))
    (
    (equal? iteracion 4)
    (cond
      (
       ( can_move2 (+ fila 1) (- columna 2) matriz largo)
       1)
      (else 0)))
    (
    (equal? iteracion 3)
    (cond
      (
       ( can_move2 (+ fila 2) (- columna 1) matriz largo)
       1)
      (else 0)))
    (
    (equal? iteracion 2)
    (cond
      (
       ( can_move2 (+ fila 2) (+ columna 1) matriz largo)
       1)
      (else 0)))
    (
    (equal? iteracion 1)
    (cond
      (
       ( can_move2 (+ fila 1) (+ columna 2) matriz largo)
       1)
      (else 0)))))
     

( define ( get_position elemento lista contador)
   (cond
     (
     (equal? lista '(99 99 99 99 99 99 99 99 ))
      99)
     (
      (equal? (car lista) elemento)
      contador)
     (else (get_position elemento (cdr lista) (+ contador 1)))))


(define (get_less lista)
  (get_less_aux (cdr lista) (car lista)))


(define (get_less_aux lista menor)
  (cond
    (
     (null? lista)
     menor
     )
    (
    ( < (car lista) menor)
    (get_less_aux (cdr lista) (car lista)))
    (else (get_less_aux (cdr lista) menor))))
 
(define (get_movimiento  lista)
  ( get_position (get_less lista) lista 1))

(define (get_best_movement fila columna matriz largo)
   (get_movimiento (append (list( calc_posiciones (- fila 1) (+ columna 2) matriz  8 largo))
                            (append  (list ( calc_posiciones (- fila 2) (+ columna 1) matriz 8 largo))
                                     (append (list (calc_posiciones (- fila 2) (- columna 1)  matriz 8 largo))
                                             (append (list ( calc_posiciones (- fila 1) (- columna 2)  matriz 8 largo))
                                                     (append (list ( calc_posiciones  (+ fila 1) (- columna 2) matriz 8 largo))
                                                             (append (list (calc_posiciones (+ fila 2) (- columna 1) matriz 8 largo))
                                                                     (append ( list ( calc_posiciones (+ fila 2) (+ columna 1) matriz 8 largo))
                                                                             (append (list (calc_posiciones (+ fila 1) (+ columna 2) matriz 8 largo)) '()))))))))))
(define (can_move2 fila columna matriz largo)
  (cond
    (
     (or (> columna (- largo 1) ) (> fila (- largo 1)))
     #f)
     (
      (or (< columna 0) (< fila 0))
      #f)
     (else (can_move fila columna matriz))))




(define (realizar_mov fila columna movimiento_mejor num_movimiento matriz largo)
  (cond
    (
     (equal? movimiento_mejor 1)
     (resolver (movimiento  (- fila 1) (+ columna 2)  matriz num_movimiento largo) (- fila 1) (+ columna 2) num_movimiento largo)
     )
    (
     (equal? movimiento_mejor 2)
     (resolver (movimiento  (- fila 2) (+ columna 1)  matriz num_movimiento largo) (- fila 2) (+ columna 1) num_movimiento largo))
    (
     (equal? movimiento_mejor 3)
     (resolver (movimiento (- fila 2) (- columna 1) matriz num_movimiento largo) (- fila 2) (- columna 1) num_movimiento largo))
    (
     (equal? movimiento_mejor 4)
     (resolver (movimiento  (- fila 1) (- columna 2)  matriz num_movimiento largo) (- fila 1) (- columna 2) num_movimiento largo))
    (
     (equal? movimiento_mejor 5)
     (resolver (movimiento  (+ fila 1) (- columna 2) matriz num_movimiento largo) (+ fila 1) (- columna 2) num_movimiento largo))
    (
     (equal? movimiento_mejor 6)
     (resolver (movimiento (+ fila 2) (- columna 1) matriz num_movimiento largo) (+ fila 2) (- columna 1) num_movimiento largo))
    (
     (equal? movimiento_mejor 7)
    (resolver (movimiento (+ fila 2) (+ columna 1)  matriz num_movimiento largo) (+ fila 2) (+ columna 1) num_movimiento largo) )
    (
     (equal? movimiento_mejor  8)
     (resolver (movimiento  (+ fila 1) (+ columna 2)  matriz num_movimiento largo) (+ fila 1) (+ columna 2) num_movimiento largo))
    (
     (equal? movimiento_mejor  99)
     '())
    
    ))

(define (resolver matriz fila columna num_movimiento largo)
  (cond
    (
    (equal? num_movimiento (* largo largo))
    matriz)
    (
    (null? matriz)
    '())
    (else
     (realizar_mov fila columna (get_best_movement fila columna matriz largo) ( + num_movimiento 1) matriz largo))))

(define (PDC-Sol n coord)
  (resolver (movimiento (car coord) (car (cdr coord)) (crear_matriz n n '()) 1 n) (car coord) (car (cdr coord)) 1 n))
  
;(resolver matriz fila columna num_movimiento largo) 
   

;;############################ Test ################################

;;(PDC-Sol 5 '(1 3))
;;(PDC-Todas 5 '(0 0))
;;(PDC-Test 5 '((21 18 9 4 1) (8 3 20 17 10) (19 22 13 2 5) (14 7 24 11 16) (23 12 15 6 25)))
(PDC-Paint 8 '((01 48 31 50 33 16 63 18) (30 51 46 03 62 19 14 35)
               (47 02 49 32 15 34 17 64) (52 29 04 45 20 61 36 13)
               (05 44 25 56 09 40 21 60) (28 53 08 41 24 57 12 37)
               (43 06 55 26 39 10 59 22) (54 27 42 07 58 23 38 11)))
;;(PDC-Paint 5 (PDC-Sol 5 '(1 3)))
;;(PDC-Paint 5 (PDC-Test 5 '((21 18 9 4 1) (8 3 20 17 10) (19 22 13 2 5) (14 7 24 11 16) (25 12 15 6 23))))
 
;;###################################################################
