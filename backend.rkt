#lang racket

;Exporta las funciones principales para la interfaz
(provide crear-tablero
         mover-izquierda
         mover-derecha
         mover-arriba
         mover-abajo
         agregar-nueva-baldosa
         hay-2048?
         hay-movimientos?
         tablero-igual?
         filas-tablero
         columnas-tablero)


;Funciones AUX
;Calcula la longitud de una lista de forma recursiva
(define (longitud lista)
  (if (null? lista)
      0
      (+ 1 (longitud (cdr lista)))))


;Devuelve la cantidad de filas del tablero
(define (filas-tablero tablero)
  (longitud tablero))


;Devuelve la cantidad de columnas del tablero
;(asume que todas las filas tienen el mismo tamaño)
(define (columnas-tablero tablero)
  (if (null? tablero)
      0
      (longitud (car tablero))))


;Compara dos tableros para verificar si son iguales
(define (tablero-igual? a b)
  (equal? a b))


;Crea una fila con valores iniciales en 0
(define (crear-fila columnas)
  (if (= columnas 0)
      '()
      (cons 0 (crear-fila (- columnas 1)))))


;Crea un tablero vacío con el número de filas y columnas especificado
(define (crear-tablero-vacio filas columnas)
  (if (= filas 0)
      '()
      (cons (crear-fila columnas)
            (crear-tablero-vacio (- filas 1) columnas))))

;Reemplaza un valor en una fila en la posición indicada
(define (reemplazar-en-fila fila columna valor)
  (if (null? fila)
      '()
      (if (= columna 0)
          (cons valor (cdr fila))
          (cons (car fila)
                (reemplazar-en-fila (cdr fila) (- columna 1) valor)))))

;Reemplaza un valor en el tablero usando coordenadas fila/columna
(define (reemplazar-en-tablero tablero fila columna valor)
  (if (null? tablero)
      '()
      (if (= fila 0)
          (cons (reemplazar-en-fila (car tablero) columna valor)
                (cdr tablero))
          (cons (car tablero)
                (reemplazar-en-tablero (cdr tablero) (- fila 1) columna valor)))))

;Convierte un índice lineal a número de fila
(define (indice-a-fila indice columnas)
  (quotient indice columnas))

;Convierte un índice lineal a número de columna
(define (indice-a-columna indice columnas)
  (remainder indice columnas))

;Coloca un valor en el tablero usando un índice lineal
(define (poner-valor-por-indice tablero columnas indice valor)
  (reemplazar-en-tablero tablero
                         (indice-a-fila indice columnas)
                         (indice-a-columna indice columnas)
                         valor))

;Genera una posición aleatoria distinta a otra ya dada
(define (generar-posicion-distinta total pos1)
  (if (= total 1)
      0
      (generar-posicion-distinta-aux total pos1 (random total))))

;Función auxiliar para asegurar que la posición generada sea distinta
(define (generar-posicion-distinta-aux total pos1 candidato)
  (if (= candidato pos1)
      (generar-posicion-distinta total pos1)
      candidato))

;Crea el tablero inicial con al menos una baldosa (2)
(define (crear-tablero filas columnas)
  (if (or (<= filas 0) (<= columnas 0))
      '()
      (crear-tablero-inicial filas columnas (* filas columnas))))

;Inicializa el tablero colocando una o dos baldosas
(define (crear-tablero-inicial filas columnas total)
  (if (= total 1)
      (poner-valor-por-indice (crear-tablero-vacio filas columnas)
                              columnas
                              0
                              2)
      (crear-tablero-con-dos filas columnas total (random total))))

;Coloca dos baldosas iniciales en posiciones distintas
(define (crear-tablero-con-dos filas columnas total posicion1)
  (poner-valor-por-indice
   (poner-valor-por-indice (crear-tablero-vacio filas columnas)
                           columnas
                           posicion1
                           2)
   columnas
   (generar-posicion-distinta total posicion1)
   2))

;Lógica para combinación 
;Elimina los ceros de una fila (compacta los valores hacia la izquierda)
(define (quitar-ceros fila)
  (if (null? fila)
      '()
      (if (= (car fila) 0)
          (quitar-ceros (cdr fila))
          (cons (car fila)
                (quitar-ceros (cdr fila))))))

;Combina valores iguales consecutivos según las reglas de 2048
;Retorna una pareja (fila-resultante . puntaje-generado)
(define (combinar fila)
  (if (null? fila)
      (cons '() 0)
      (if (null? (cdr fila))
          (cons fila 0)
          (if (= (car fila) (car (cdr fila)))
              (cons
               (cons (* 2 (car fila))
                     (car (combinar (cdr (cdr fila)))))
               (+ (* 2 (car fila))
                  (cdr (combinar (cdr (cdr fila))))))
              (cons
               (cons (car fila)
                     (car (combinar (cdr fila))))
               (cdr (combinar (cdr fila))))))))

;Rellena una fila con ceros hasta alcanzar el tamaño original
(define (rellenar-con-ceros fila tamano)
  (if (= (longitud fila) tamano)
      fila
      (rellenar-con-ceros (append fila '(0)) tamano)))

;Procesa una fila completa: quita ceros, combina y rellena
(define (procesar-fila fila tamano)
  (cons
   (rellenar-con-ceros (car (combinar (quitar-ceros fila))) tamano)
   (cdr (combinar (quitar-ceros fila)))))


;Lógica para los movimientos de forma horizontal
;Aplica el movimiento hacia la izquierda a todo el tablero
(define (mover-izquierda tablero)
  (if (null? tablero)
      (cons '() 0)
      (cons
       (cons (car (procesar-fila (car tablero) (longitud (car tablero))))
             (car (mover-izquierda (cdr tablero))))
       (+ (cdr (procesar-fila (car tablero) (longitud (car tablero))))
          (cdr (mover-izquierda (cdr tablero)))))))

;Invierte una lista (utilizado para mover a la derecha)
(define (reverse-lista lista)
  (if (null? lista)
      '()
      (append (reverse-lista (cdr lista))
              (list (car lista)))))

;Invierte cada fila del tablero
(define (invertir-tablero tablero)
  (if (null? tablero)
      '()
      (cons (reverse-lista (car tablero))
            (invertir-tablero (cdr tablero)))))

;Movimiento hacia la derecha (invertir + izquierda + invertir)
(define (mover-derecha tablero)
  (cons
   (invertir-tablero (car (mover-izquierda (invertir-tablero tablero))))
   (cdr (mover-izquierda (invertir-tablero tablero)))))

;; ==========================================
;; TRANSPOSICIÓN Y MOVIMIENTOS VERTICALES
;; ==========================================
;Transfrmación y movimientos verticales
;Obtiene la primera columna del tablero
(define (primeros tablero)
  (if (null? tablero)
      '()
      (cons (car (car tablero))
            (primeros (cdr tablero)))))


;Obtiene el resto de columnas del tablero
(define (restos tablero)
  (if (null? tablero)
      '()
      (cons (cdr (car tablero))
            (restos (cdr tablero)))))


;Transpone el tablero (filas ↔ columnas)
(define (transponer tablero)
  (if (or (null? tablero) (null? (car tablero)))
      '()
      (cons (primeros tablero)
            (transponer (restos tablero)))))


;Movimiento hacia arriba (transponer + izquierda)
(define (mover-arriba tablero)
  (cons
   (transponer (car (mover-izquierda (transponer tablero))))
   (cdr (mover-izquierda (transponer tablero)))))


;Movimiento hacia abajo (transponer + derecha)
(define (mover-abajo tablero)
  (cons
   (transponer (car (mover-derecha (transponer tablero))))
   (cdr (mover-derecha (transponer tablero)))))


;Lógica para agrega nuevas baldozas
;Encuentra posiciones vacías (0) en una fila
(define (posiciones-vacias-fila fila indice)
  (if (null? fila)
      '()
      (if (= (car fila) 0)
          (cons indice
                (posiciones-vacias-fila (cdr fila) (+ indice 1)))
          (posiciones-vacias-fila (cdr fila) (+ indice 1)))))

;Encuentra todas las posiciones vacías del tablero
(define (posiciones-vacias tablero indice-inicial)
  (if (null? tablero)
      '()
      (append (posiciones-vacias-fila (car tablero) indice-inicial)
              (posiciones-vacias (cdr tablero)
                                 (+ indice-inicial (longitud (car tablero)))))))

;Genera aleatoriamente un 2 o un 4
(define (valor-aleatorio)
  (if (= (random 2) 0)
      2
      4))

;Agrega una nueva baldosa en una posición vacía aleatoria
(define (agregar-nueva-baldosa tablero)
  (if (null? (posiciones-vacias tablero 0))
      tablero
      (poner-valor-por-indice tablero
                              (columnas-tablero tablero)
                              (list-ref (posiciones-vacias tablero 0)
                                        (random (longitud (posiciones-vacias tablero 0))))
                              (valor-aleatorio))))


;Cuando gana y pierde
;Verifica si existe un 2048 en una fila
(define (hay-2048-fila? fila)
  (if (null? fila)
      #f
      (if (= (car fila) 2048)
          #t
          (hay-2048-fila? (cdr fila)))))

;Verifica si el jugador ganó (existe un 2048 en el tablero)
(define (hay-2048? tablero)
  (if (null? tablero)
      #f
      (if (hay-2048-fila? (car tablero))
          #t
          (hay-2048? (cdr tablero)))))

;Verifica si hay espacios vacíos en una fila
(define (hay-cero-fila? fila)
  (if (null? fila)
      #f
      (if (= (car fila) 0)
          #t
          (hay-cero-fila? (cdr fila)))))

;Verifica si hay espacios vacíos en el tablero
(define (hay-cero? tablero)
  (if (null? tablero)
      #f
      (if (hay-cero-fila? (car tablero))
          #t
          (hay-cero? (cdr tablero)))))

;Determina si aún hay movimientos posibles
;(ya sea por espacios vacíos o combinaciones posibles)
(define (hay-movimientos? tablero)
  (if (hay-cero? tablero)
      #t
      (if (tablero-igual? tablero (car (mover-izquierda tablero)))
          (if (tablero-igual? tablero (car (mover-derecha tablero)))
              (if (tablero-igual? tablero (car (mover-arriba tablero)))
                  (not (tablero-igual? tablero (car (mover-abajo tablero))))
                  #t)
              #t)
          #t)))