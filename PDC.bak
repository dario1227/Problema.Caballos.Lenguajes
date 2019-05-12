;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Trabajo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
(define ( movimiento n1 n2 matriz numero)
  ( cond (
          (or (> n1 (- (largo matriz) 1)) (> n2 (- (largo matriz) 1)))
          '() )
         (
          (or (< n1 0) (< n2 0))
          '() )
         (
           (can_move n1 n2 matriz)
          (movimiento_aux n1 n2 matriz '() (largo matriz) numero ))
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
  (resolver_todas  (movimiento  (car coordenada) (car (cdr coordenada)) (crear_matriz n n '()) 1) 1 (car coordenada) (car (cdr coordenada)) ))

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

(define (resolver_todas matriz num_movimiento mov_x mov_y)
  (cond(
        (null? matriz)
        
        '())
        (
         (resuelta? matriz)
        (list matriz))
        (else (append (append (append (append (append ( append ( append (append
                       '()
                      (resolver_todas  (movimiento  (+ 2 mov_x) (+ 1 mov_y)  matriz (+ 1 num_movimiento)) (+ 1 num_movimiento) (+ 2 mov_x) (+ 1 mov_y) ))
                      (resolver_todas  (movimiento  (+ 1 mov_x) (+ 2 mov_y)  matriz (+ 1 num_movimiento)) (+ 1 num_movimiento) (+ 1 mov_x) (+ 2 mov_y) ))
                      (resolver_todas  (movimiento  (+ 2 mov_x) (- mov_y 1)  matriz (+ 1 num_movimiento)) (+ 1 num_movimiento) (+ 2 mov_x) (- mov_y 1) ) )
                      (resolver_todas  (movimiento  (+ 1 mov_x) (- mov_y 2)  matriz (+ 1 num_movimiento)) (+ 1 num_movimiento) (+ 1 mov_x) (- mov_y 2) ))
                      (resolver_todas  (movimiento  (- mov_x 1) (+ mov_y 2)  matriz (+ 1 num_movimiento)) (+ 1 num_movimiento) (- mov_x 1) (+ mov_y 2) ))
                      (resolver_todas  (movimiento  (- mov_x 2) (+ mov_y 1)  matriz (+ 1 num_movimiento)) (+ 1 num_movimiento) (- mov_x 2) (+ mov_y 1) ))
                      (resolver_todas  (movimiento  (- mov_x 1) (- mov_y 2)  matriz (+ 1 num_movimiento)) (+ 1 num_movimiento) (- mov_x 1) (- mov_y 2) ))
                      (resolver_todas  (movimiento  (- mov_x 2) (- mov_y 1)  matriz (+ 1 num_movimiento)) (+ 1 num_movimiento) (- mov_x 2) (- mov_y 1) )))))


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
  ( comprueba_sol (crear_matriz n n '()) sol_matriz (search_value 0 1 sol_matriz) (search_value 0 2 sol_matriz) 1))



(define (comprueba_sol matriz solucion primer_coordenada segunda_coordenada elemento_actual )
  (cond
    (
     (null? segunda_coordenada)
    (movimiento  (car primer_coordenada)  (car (cdr primer_coordenada))  matriz elemento_actual))
       (
        (null? primer_coordenada)
        matriz
        )
       (
        (valido? primer_coordenada segunda_coordenada)
        (comprueba_sol (movimiento  (car primer_coordenada)  (car (cdr primer_coordenada))  matriz elemento_actual)
                       solucion segunda_coordenada (search_value  0 (+ elemento_actual 2) solucion) (+ elemento_actual 1)
                       ))
       (else (movimiento  (car primer_coordenada)  (car (cdr primer_coordenada))  matriz elemento_actual)
        )))


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


(define (PDC-Sol n posicion)
  ( crear_matriz_pasos (movimiento  (car posicion) (car (cdr posicion)) (crear_matriz n n '()) 1) 1 (car posicion) (car (cdr posicion) )  '()))



(define ( crear_matriz_pasos matriz actual fila columna resultado)
  (cond
        (
     (null? matriz)
     '( )
     )
    (
     (resuelta? matriz)
     (append resultado (list matriz)))

    (
     (not (null?  ( crear_matriz_pasos (movimiento  (+ fila 2) (+ columna 1) matriz (+ actual 1) ) (+ actual 1) (+ fila 2) (+ columna 1)  (append resultado (list matriz)) ) ) )
     ( crear_matriz_pasos (movimiento  (+ fila 2) (+ columna 1) matriz (+ actual 1) ) (+ actual 1) (+ fila 2) (+ columna 1)  (append resultado (list matriz)) ))
    (
     (not (null?  ( crear_matriz_pasos (movimiento  (+ fila 1) (+ columna 2) matriz (+ actual 1) ) (+ actual 1) (+ fila 1) (+ columna 2)  (append resultado (list matriz)) ) ) )
     ( crear_matriz_pasos (movimiento  (+ fila 1) (+ columna 2) matriz (+ actual 1) ) (+ actual 1) (+ fila 1) (+ columna 2)  (append resultado (list matriz)) ))
    (
     (not (null?  ( crear_matriz_pasos (movimiento  (+ fila 2) (- columna 1) matriz (+ actual 1) ) (+ actual 1) (+ fila 2) (- columna 1)  (append resultado (list matriz)) ) ) )
     ( crear_matriz_pasos (movimiento  (+ fila 2) (- columna 1) matriz (+ actual 1) ) (+ actual 1) (+ fila 2) (- columna 1)  (append resultado (list matriz)) ))
    (
     (not (null?  ( crear_matriz_pasos (movimiento  (+ fila 1) (- columna 2)  matriz (+ actual 1) ) (+ actual 1) (+ fila 1) (- columna 2)  (append resultado (list matriz)) ) ) )
     ( crear_matriz_pasos (movimiento  (+ fila 1) (- columna 2)  matriz (+ actual 1) ) (+ actual 1) (+ fila 1) (- columna 2)  (append resultado (list matriz)) ))
    (
     (not (null?  ( crear_matriz_pasos (movimiento  (- fila 1) (+ columna 2) matriz (+ actual 1) ) (+ actual 1) (- fila 1) (+ columna 2)  (append resultado (list matriz)) ) ) )
     ( crear_matriz_pasos (movimiento  (- fila 1) (+ columna 2) matriz (+ actual 1) ) (+ actual 1) (- fila 1) (+ columna 2)  (append resultado (list matriz)) ))
    (
     (not (null?  ( crear_matriz_pasos (movimiento  (- fila 2) (+ columna 1) matriz (+ actual 1) ) (+ actual 1) (- fila 2) (+ columna 1)  (append resultado (list matriz)) ) ) )
     ( crear_matriz_pasos (movimiento  (- fila 2) (+ columna 1) matriz (+ actual 1) ) (+ actual 1) (- fila 2) (+ columna 1)  (append resultado (list matriz)) ))
    (
     (not (null?  ( crear_matriz_pasos (movimiento  (- fila 1) (- columna 2) matriz (+ actual 1) ) (+ actual 1) (- fila 1) (- columna 2)  (append resultado (list matriz)) ) ) )
     ( crear_matriz_pasos (movimiento  (- fila 1) (- columna 2) matriz (+ actual 1) ) (+ actual 1) (- fila 2) (- columna 2)  (append resultado (list matriz)) ))
    (
     (not (null?  ( crear_matriz_pasos (movimiento  (-  fila 2)  (- columna 1) matriz (+ actual 1) ) (+ actual 1) (-  fila 2) (- columna 1)  (append resultado (list matriz)) ) ) )
     ( crear_matriz_pasos (movimiento  (-  fila 2)  (- columna 1) matriz (+ actual 1) ) (+ actual 1) (- fila 2) (- columna 1)  (append resultado (list matriz)) ))
    (else '() )))       


     
         

         
                      

        

  