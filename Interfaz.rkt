#lang racket
;; Tarea Progamada Problema del Caballo
;; Prof: Marco Vinicio
;; Alumnos: Dario Rodríguez
;;          Jonathan Guzmán
;;          Kenneth Hernández

;;##########################################################################Definición de variables para GUI#########################################################################

(require racket/gui/base) ;; Se importa la librería graphics para desarrollar la interfaz gráfica.
(require (lib "graphics.ss" "graphics")) ;; Se importa la librería graphics para desarrollar la interfaz gráfica.
(open-graphics) ;; Se abre el modo gráfico de DrRacket.

(define wide 640) ;; Ancho de la ventana.
(define height 640) ;; Alto de la ventana.
(define borde 0) ;; Borde de la ventana

(define horseWindow (open-viewport "ChessHorse" wide height)) ;; Se crea la ventana, se define su nombre y tamaño.
(define horseBackup (open-pixmap "ChessHorse" wide height)) ;; Se crea la ventana de respaldo, se define su nombre y tamaño.

(define u 0) ;; Intercalar los cuadros.
(define h 0) ;; Posicion del tablero en X.
(define v 0) ;; Posicion del tablero en Y.

(define verde "DarkGreen");; Cuadros verdes.
(define azul "blue");; Cuadro Azul

;;###################################################################################################################################################################################

;;##################################################################################### PDC-Paint ###################################################################################

(define (PDC-Paint n matrixSol) ;; Función que recibe un tamaño de matriz y una solución que pintará en una ventana mostrando su recorrido.
  (game n) ;; Inicia la función game para mostrar la ventana.
  (PDC-PaintAux matrixSol n 1)) ;; Llama a la función auxiliar.
  ;;(PDC-PaintAux (PDC-Test n matrixSol) n 1)) ;; Llama a la función auxiliar.


(define (PDC-PaintAux matrixSol n m) ;; Función auxiliar.
  (cond (( > m (* n n)) (display "Endgame")) ;; Si llega al final de la solución, imprime en consola un mensaje de finalización.
        ;; Busca la posición i j siguiente en la ruta y lo dibuja en la ventana.
        (else
         (horseMovement n (second(findPM matrixSol 0 0 #f m)) (first(findPM matrixSol 0 0 #f m)) verde)
         (sleep 1)(horseMovement n (second(findPM matrixSol 0 0 #f m)) (first(findPM matrixSol 0 0 #f m)) azul) (PDC-PaintAux matrixSol n (+ m 1)))))

(provide PDC-Paint)

;;###################################################################################################################################################################################

;;##################################################################################### Tablero #####################################################################################

(define (game n) ;; Función que se encarga de dibujar la cuadrícula según el tamanño de la matriz en la ventana. 
; Para h iniciando en borde, incrementanto (wide / n) y terminando en wide.
(for ((h (in-range borde wide (/ (- wide (* borde 2)) n))))
  ;; Dibuja en la variable horseWindow (ventana) inicie h en X = borde y termine h en Y = (wide - borde) con azul.
  ((draw-line horseBackup) (make-posn h borde) (make-posn h (- height borde)) "blue"))

; Para v iniciando en borde, incrementanto (height / n) y terminando en height.
(for ((v (in-range borde height (/ (- height (* borde 2)) n))))
  ;; Dibuja en la variable horseWindow (ventana) inicie v en X = borde y termine v en Y = (height - borde) con azul.
  ((draw-line horseBackup) (make-posn borde v) (make-posn (- wide borde) v) "blue")))

;;###################################################################################################################################################################################

;;################################################################################### Pinta cuadro ##################################################################################

(define (horseMovement n i j color) ;; Función que se encarga de pintar un cuadro verde en las posiciones hacia donde se desplaza el caballo.
  ((draw-solid-rectangle horseBackup) (make-posn  (* i (/ wide n))(* j (/ height n))) (- (/ wide n) 1) (- (/ height n) 1) color) ;; Dibuja un rectángulo en una posición i j dada.
  (copy-viewport horseBackup horseWindow)) ; Se copia lo de la ventana de respaldo donde realiza los movimentos nuevos a la ventana principal donde muestra todo el recorrido.

;;###################################################################################################################################################################################

;;###################################################################################### Posición ###################################################################################

(define (findPM myMatrix i j has num) ;; Como la matriz es básicamente, una lista de listas, llamará a findPL por cada elemento de myMatrix.
  (cond ((eleExist (car myMatrix) has num) (append (list i) (append '() (list (findPL (car myMatrix) 0 num))))) ;; Si encuentra al número agrega su posición en la matriz.
        (else (findPM (cdr myMatrix) (+ i 1) j has num)))) ;; Si no, se llama a si misma recursivamente y busca en la siguiente fila.

(define (findPL myList n num) ;; n es el contador de la lista, el cual tomará el valor de la posición del elemento a buscar, num es el número a buscar la posición.
  (cond ((empty? myList) n) ;; Si la lista se encuentra vacía retorna el valor de la posición en la que iba realizando la búsqueda.
        (else (cond ((equal? (car myList) num) n) ;; Si el primer elemento de la lista es igual a num, retorna el valor de n en ese momento el cual será su posición.
                    (else (findPL (cdr myList) (+ n 1) num)))))) ;; Si no lo ha encontrado, llama a sí misma pero con n+1, para así encontrar el valor de la posición.

;;###################################################################################################################################################################################

;;###################################################################################### Verifica ###################################################################################

(define (eleExist myList has num) ;; Función que verifica si un elemento se encuentra en una lista.
  (cond ((empty? myList) has) ;; Retorna falso pues no encontró el elemento dentro de la lista.
        (else (cond ((equal? (car myList) num) #t) ;; Si esta el elemento dentro de la lista, retorna true de inmediato.
                    ((equal? (car myList) 0) (sleep 2) (close-viewport horseWindow))
                    (else(eleExist (cdr myList) has num)))))) ;; Si no, se llama recursivamente pero con el resto de la lista.

;;###################################################################################################################################################################################