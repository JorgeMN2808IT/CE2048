#lang racket

(provide creaTablero
         moverIzquierda
         moverDerecha
         moverArriba
         moverAbajo
         agregar-nueva-baldosa
         hay2048?
         haymovimientos?
         tableroIgual?
         filasTablero
         columnasTablero)


;funcion para calcular la longitud de una lista
(define (longitud lista)
  (if (null? lista) ;caso base lista vacia
      0
      (+ 1 (longitud (cdr lista))))) ;cuenta elemento y sigue


;devuelve la cantidad de filas del tablero
(define (filasTablero tablero)
  (longitud tablero)) ;cada elemento del tablero es una fila


;devuelve la cantidad de columnas del tablero
(define (columnasTablero tablero)
  (if (null? tablero)
      0
      (longitud (car tablero)))) ;toma la primera fila para saber columnas


;crea una fila llena de ceros
(define (crear-fila columnas)
  (if (= columnas 0) ;cuando ya no faltan columnas
      '()
      (cons 0 (crear-fila (- columnas 1))))) ;agrega un 0 y sigue


;crea un tablero vacio lleno de ceros
(define (crear-tablero-vacio filas columnas)
  (if (= filas 0) ;cuando ya no faltan filas
      '()
      (cons (crear-fila columnas) ;crea una fila
            (crear-tablero-vacio (- filas 1) columnas)))) ;repite para todas


;compara dos tableros
(define (tableroIgual? tablero1 tablero2)
  (equal? tablero1 tablero2)) ;compara listas completas


;reemplaza un valor dentro de una fila
(define (reemplazar-en-fila fila columna valor)
  (if (null? fila)
      '()
      (if (= columna 0) ;cuando llega a la posicion deseada
          (cons valor (cdr fila)) ;pone el nuevo valor
          (cons (car fila)
                (reemplazar-en-fila (cdr fila) (- columna 1) valor)))))


;reemplaza un valor dentro del tablero
(define (reemplazar-en-tablero tablero fila columna valor)
  (if (null? tablero)
      '()
      (if (= fila 0) ;cuando llega a la fila correcta
          (cons (reemplazar-en-fila (car tablero) columna valor)
                (cdr tablero)) ;solo cambia esa fila
          (cons (car tablero)
                (reemplazar-en-tablero (cdr tablero) (- fila 1) columna valor)))))


;coloca un valor usando indice lineal
(define (colocar-valor-por-indice tablero columnas indice valor)
  (reemplazar-en-tablero tablero
                         (quotient indice columnas) ;convierte indice a fila
                         (remainder indice columnas) ;convierte indice a columna
                         valor))


;crea tablero inicial con dos numeros 2
(define (creaTablero filas columnas)
  (if (or (<= filas 0) (<= columnas 0)) ;validacion
      '()
      (crear-tablero-inicial filas columnas (* filas columnas)))) ;total de casillas


(define (crear-tablero-inicial filas columnas total)
  (if (= total 1)
      (colocar-valor-por-indice (crear-tablero-vacio filas columnas)
                                columnas
                                0
                                2)
      (crear-tablero-inicial-dos filas columnas total (random total)))) ;elige posicion random


(define (crear-tablero-inicial-dos filas columnas total posicion1)
  (colocar-valor-por-indice
   (colocar-valor-por-indice (crear-tablero-vacio filas columnas)
                             columnas
                             posicion1
                             2) ;pone primer 2
   columnas
   (generar-posicion-distinta total posicion1) ;busca otra posicion
   2)) ;pone segundo 2


;genera posicion distinta para no repetir
(define (generar-posicion-distinta total posicion)
  (if (= total 1)
      0
      (generar-posicion-distinta-aux total posicion (random total)))) ;genera candidato random


(define (generar-posicion-distinta-aux total posicion candidato)
  (if (= candidato posicion) ;si es igual a la anterior
      (generar-posicion-distinta total posicion) ;vuelve a intentar
      candidato)) ;si es diferente lo usa


;elimina los ceros de una fila
(define (quitar-ceros fila)
  (if (null? fila)
      '()
      (if (= (car fila) 0)
          (quitar-ceros (cdr fila)) ;ignora ceros
          (cons (car fila)
                (quitar-ceros (cdr fila)))))) ;mantiene los valores


;cmbina numeros iguales
(define (combinar fila)
  (if (null? fila)
      (cons '() 0) ;retorna lista vacia y puntaje 0
      (if (null? (cdr fila))
          (cons fila 0) ;solo un elemento
          (if (= (car fila) (car (cdr fila))) ;si los dos primeros son iguales
              (combinar-iguales fila)
              (combinar-distintos fila)))))


(define (combinar-iguales fila)
  (cons (cons (* 2 (car fila)) ;duplica el numero
              (car (combinar (cdr (cdr fila))))) ;continua con el resto
        (+ (* 2 (car fila)) ;suma puntos
           (cdr (combinar (cdr (cdr fila)))))))


(define (combinar-distintos fila)
  (cons (cons (car fila) ;mantiene el numero
              (car (combinar (cdr fila))))
        (cdr (combinar (cdr fila)))))


;rellena la fila con ceros al final
(define (rellenar-con-ceros fila tamano)
  (if (= (longitud fila) tamano)
      fila
      (rellenar-con-ceros (append fila '(0)) tamano))) ;agrega ceros hasta completar


;procesa una fila completa
(define (procesar-fila fila tamano)
  (cons (rellenar-con-ceros (car (combinar (quitar-ceros fila))) tamano) ;nueva fila
        (cdr (combinar (quitar-ceros fila))))) ;puntaje generado


;mueve todo el tablero a la izquierda
(define (moverIzquierda tablero)
  (if (null? tablero)
      (cons '() 0)
      (unir-movimientos (procesar-fila (car tablero) (longitud (car tablero))) ;procesa fila
                        (moverIzquierda (cdr tablero))))) ;continua con el resto


;une los resultados de cada fila
(define (unir-movimientos fila-resultado resto-resultado)
  (cons (cons (car fila-resultado)
              (car resto-resultado)) ;arma nuevo tablero
        (+ (cdr fila-resultado)
           (cdr resto-resultado)))) ;suma todos los puntajes


;invierte una lista
(define (reverse-lista lista)
  (if (null? lista)
      '()
      (append (reverse-lista (cdr lista))
              (list (car lista))))) ;pone el primero al final


;invierte cada fila del tablero
(define (invertir-tablero tablero)
  (if (null? tablero)
      '()
      (cons (reverse-lista (car tablero)) ;invierte fila
            (invertir-tablero (cdr tablero)))))


;obtiene primera columna
(define (primeros tablero)
  (if (null? tablero)
      '()
      (cons (car (car tablero)) ;primer elemento de cada fila
            (primeros (cdr tablero))))


;obtiene resto de columnas
(define (restos tablero)
  (if (null? tablero)
      '()
      (cons (cdr (car tablero)) ;quita el primero
            (restos (cdr tablero)))))


;transpone el tablero
(define (transponer tablero)
  (if (or (null? tablero) (null? (car tablero)))
      '()
      (cons (primeros tablero) ;columna pasa a fila
            (transponer (restos tablero)))))


;mueve a la derecha
(define (moverDerecha tablero)
  (reconstruir-movimiento-derecha (moverIzquierda (invertir-tablero tablero)))) ;truco: invertir + izquierda


(define (reconstruir-movimiento-derecha resultado)
  (cons (invertir-tablero (car resultado)) ;vuelve a invertir
        (cdr resultado)))


;mueve hacia arriba
(define (moverArriba tablero)
  (reconstruir-movimiento-vertical (moverIzquierda (transponer tablero)))) ;usa transpuesta


;mueve hacia abajo
(define (moverAbajo tablero)
  (reconstruir-movimiento-vertical (moverDerecha (transponer tablero)))) ;igual pero derecha


(define (reconstruir-movimiento-vertical resultado)
  (cons (transponer (car resultado)) ;regresa a forma original
        (cdr resultado)))

