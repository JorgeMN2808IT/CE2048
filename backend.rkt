#lang racket

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

;; ==========================================
;; UTILIDADES GENERALES
;; ==========================================

(define (longitud lista)
  (if (null? lista)
      0
      (+ 1 (longitud (cdr lista)))))

(define (filas-tablero tablero)
  (longitud tablero))

(define (columnas-tablero tablero)
  (if (null? tablero)
      0
      (longitud (car tablero))))

(define (tablero-igual? a b)
  (equal? a b))

;; ==========================================
;; CREACIÓN DEL TABLERO
;; ==========================================

(define (crear-fila columnas)
  (if (= columnas 0)
      '()
      (cons 0 (crear-fila (- columnas 1)))))

(define (crear-tablero-vacio filas columnas)
  (if (= filas 0)
      '()
      (cons (crear-fila columnas)
            (crear-tablero-vacio (- filas 1) columnas))))

(define (reemplazar-en-fila fila columna valor)
  (if (null? fila)
      '()
      (if (= columna 0)
          (cons valor (cdr fila))
          (cons (car fila)
                (reemplazar-en-fila (cdr fila) (- columna 1) valor)))))

(define (reemplazar-en-tablero tablero fila columna valor)
  (if (null? tablero)
      '()
      (if (= fila 0)
          (cons (reemplazar-en-fila (car tablero) columna valor)
                (cdr tablero))
          (cons (car tablero)
                (reemplazar-en-tablero (cdr tablero) (- fila 1) columna valor)))))

(define (indice-a-fila indice columnas)
  (quotient indice columnas))

(define (indice-a-columna indice columnas)
  (remainder indice columnas))

(define (poner-valor-por-indice tablero columnas indice valor)
  (reemplazar-en-tablero tablero
                         (indice-a-fila indice columnas)
                         (indice-a-columna indice columnas)
                         valor))

(define (generar-posicion-distinta total pos1)
  (if (= total 1)
      0
      (generar-posicion-distinta-aux total pos1 (random total))))

(define (generar-posicion-distinta-aux total pos1 candidato)
  (if (= candidato pos1)
      (generar-posicion-distinta total pos1)
      candidato))

(define (crear-tablero filas columnas)
  (if (or (<= filas 0) (<= columnas 0))
      '()
      (crear-tablero-inicial filas columnas (* filas columnas))))

(define (crear-tablero-inicial filas columnas total)
  (if (= total 1)
      (poner-valor-por-indice (crear-tablero-vacio filas columnas)
                              columnas
                              0
                              2)
      (crear-tablero-con-dos filas columnas total (random total))))

(define (crear-tablero-con-dos filas columnas total posicion1)
  (poner-valor-por-indice
   (poner-valor-por-indice (crear-tablero-vacio filas columnas)
                           columnas
                           posicion1
                           2)
   columnas
   (generar-posicion-distinta total posicion1)
   2))

;; ==========================================
;; LÓGICA DE COMBINACIÓN
;; ==========================================

(define (quitar-ceros fila)
  (if (null? fila)
      '()
      (if (= (car fila) 0)
          (quitar-ceros (cdr fila))
          (cons (car fila)
                (quitar-ceros (cdr fila))))))

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

(define (rellenar-con-ceros fila tamano)
  (if (= (longitud fila) tamano)
      fila
      (rellenar-con-ceros (append fila '(0)) tamano)))

(define (procesar-fila fila tamano)
  (cons
   (rellenar-con-ceros (car (combinar (quitar-ceros fila))) tamano)
   (cdr (combinar (quitar-ceros fila)))))

;; ==========================================
;; MOVIMIENTOS HORIZONTALES
;; ==========================================

(define (mover-izquierda tablero)
  (if (null? tablero)
      (cons '() 0)
      (cons
       (cons (car (procesar-fila (car tablero) (longitud (car tablero))))
             (car (mover-izquierda (cdr tablero))))
       (+ (cdr (procesar-fila (car tablero) (longitud (car tablero))))
          (cdr (mover-izquierda (cdr tablero)))))))

(define (reverse-lista lista)
  (if (null? lista)
      '()
      (append (reverse-lista (cdr lista))
              (list (car lista)))))

(define (invertir-tablero tablero)
  (if (null? tablero)
      '()
      (cons (reverse-lista (car tablero))
            (invertir-tablero (cdr tablero)))))

(define (mover-derecha tablero)
  (cons
   (invertir-tablero (car (mover-izquierda (invertir-tablero tablero))))
   (cdr (mover-izquierda (invertir-tablero tablero)))))

;; ==========================================
;; TRANSPOSICIÓN Y MOVIMIENTOS VERTICALES
;; ==========================================

(define (primeros tablero)
  (if (null? tablero)
      '()
      (cons (car (car tablero))
            (primeros (cdr tablero)))))

(define (restos tablero)
  (if (null? tablero)
      '()
      (cons (cdr (car tablero))
            (restos (cdr tablero)))))

(define (transponer tablero)
  (if (or (null? tablero) (null? (car tablero)))
      '()
      (cons (primeros tablero)
            (transponer (restos tablero)))))

(define (mover-arriba tablero)
  (cons
   (transponer (car (mover-izquierda (transponer tablero))))
   (cdr (mover-izquierda (transponer tablero)))))

(define (mover-abajo tablero)
  (cons
   (transponer (car (mover-derecha (transponer tablero))))
   (cdr (mover-derecha (transponer tablero)))))

;; ==========================================
;; AGREGAR NUEVA BALDOSA
;; ==========================================

(define (posiciones-vacias-fila fila indice)
  (if (null? fila)
      '()
      (if (= (car fila) 0)
          (cons indice
                (posiciones-vacias-fila (cdr fila) (+ indice 1)))
          (posiciones-vacias-fila (cdr fila) (+ indice 1)))))

(define (posiciones-vacias tablero indice-inicial)
  (if (null? tablero)
      '()
      (append (posiciones-vacias-fila (car tablero) indice-inicial)
              (posiciones-vacias (cdr tablero)
                                 (+ indice-inicial (longitud (car tablero)))))))

(define (valor-aleatorio)
  (if (= (random 2) 0)
      2
      4))

(define (agregar-nueva-baldosa tablero)
  (if (null? (posiciones-vacias tablero 0))
      tablero
      (poner-valor-por-indice tablero
                              (columnas-tablero tablero)
                              (list-ref (posiciones-vacias tablero 0)
                                        (random (longitud (posiciones-vacias tablero 0))))
                              (valor-aleatorio))))

;; ==========================================
;; DETECCIÓN DE GANAR Y PERDER
;; ==========================================

(define (hay-2048-fila? fila)
  (if (null? fila)
      #f
      (if (= (car fila) 2048)
          #t
          (hay-2048-fila? (cdr fila)))))

(define (hay-2048? tablero)
  (if (null? tablero)
      #f
      (if (hay-2048-fila? (car tablero))
          #t
          (hay-2048? (cdr tablero)))))

(define (hay-cero-fila? fila)
  (if (null? fila)
      #f
      (if (= (car fila) 0)
          #t
          (hay-cero-fila? (cdr fila)))))

(define (hay-cero? tablero)
  (if (null? tablero)
      #f
      (if (hay-cero-fila? (car tablero))
          #t
          (hay-cero? (cdr tablero)))))

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