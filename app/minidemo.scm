(newline)
(display "*****************************************************") (newline)
(display "*                    SCHEME 2021                    *") (newline)
(display "* DEMO DE DEFINICION Y USO DE VARIABLES Y FUNCIONES *") (newline)
(display "*****************************************************") (newline)
(newline)
(display "OBS.: SCHEME NO DISTINGUE MAYUSCULAS DE MINUSCULAS.  ") (newline)
(display "      PARA APROVECHAR ESTA CARACTERISTICA, ALGUNAS   ") (newline)
(display "      VARIABLES Y FUNCIONES SE DEFINIERON A PROPOSITO") (newline)
(display "      EN MAYUSCULAS Y OTRAS EN MINUSCULAS.")            (newline)

(newline)
(display "IOTA [RETORNA UNA LISTA CON LOS PRIMEROS N NUMEROS NATURALES]:") (newline)

(display "> (DEFINE (IOTA N)") (newline)
(display "    (IF (< N 1)") (newline)
(display "         ()") (newline)
(display "         (AUXIOTA 1 N)))") (newline)

(display
(DEFINE (IOTA N)
    (IF (< N 1)
     ()
     (AUXIOTA 1 N)))
) (newline)

(display "> (DEFINE (AUXIOTA I N)") (newline)
(display "    (IF (EQUAL? I N)") (newline)
(display "        (LIST N)") (newline)
(display "        (CONS I (AUXIOTA (+ I 1) N))))") (newline)

(display
(DEFINE (AUXIOTA I N)
  (IF (EQUAL? I N)
    (LIST N)
    (CONS I (AUXIOTA (+ I 1) N))))
) (newline)

(display "> (iota 10)") (newline)
(display (iota 10)) (newline)

(newline)
(display "FUNCIONES IMPLEMENTADAS USANDO LAS FUNCIONES ANTERIORES") (newline)
(display "-------------------------------------------------------") (newline)

(newline)


(display "> (length '(A B C D E F G H I J))") (newline)
(display  (length '(A B C D E F G H I J)))

(newline)

(display "> (iota (length '(A B C D E F G H I J)))") (newline)
(display (iota (length '(A B C D E F G H I J))))