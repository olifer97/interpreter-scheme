(ns app.core)
;(load-file "src/app/scheme.clj")
(require '[clojure.string :as st :refer [blank? starts-with? ends-with? lower-case]]
         '[clojure.java.io :refer [delete-file reader]]
         '[clojure.walk :refer [postwalk postwalk-replace]])

(defn spy
  ([x] (do (prn x) x))
  ([msg x] (do (print msg) (print ": ") (prn x) x)))

; Funciones principales
(declare repl)
(declare evaluar)
(declare aplicar)

; Funciones secundarias de evaluar
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond)
(declare evaluar-eval)
(declare evaluar-exit)
(declare evaluar-load)
(declare evaluar-set!)
(declare evaluar-quote)
(declare evaluar-define)
(declare evaluar-lambda)
(declare evaluar-escalar)

; Funciones secundarias de aplicar
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva)

; Funciones primitivas
(declare fnc-car)
(declare fnc-cdr)
(declare fnc-env)
(declare fnc-not)
(declare fnc-cons)
(declare fnc-list)
(declare fnc-list?)
(declare fnc-read)
(declare fnc-mayor)
(declare fnc-menor)
(declare fnc-null?)
(declare fnc-sumar)
(declare fnc-append)
(declare fnc-equal?)
(declare fnc-length)
(declare fnc-restar)
(declare fnc-display)
(declare fnc-newline)
(declare fnc-reverse)
(declare fnc-mayor-o-igual)

; Funciones auxiliares

(declare buscar)
(declare error?)
(declare igual?)
(declare imprimir)
(declare cargar-arch)
(declare revisar-fnc)
(declare revisar-lae)
(declare leer-entrada)
(declare actualizar-amb)
(declare restaurar-bool)
(declare generar-nombre-arch)
(declare nombre-arch-valido?)
(declare controlar-aridad-fnc)
(declare proteger-bool-en-str)
(declare verificar-parentesis)
(declare generar-mensaje-error)
(declare aplicar-lambda-simple)
(declare aplicar-lambda-multiple)
(declare evaluar-clausulas-de-cond)
(declare evaluar-secuencia-en-cond)

; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra > y lee una expresion y la evalua. El resultado es una lista con un valor y un ambiente. 
; Si la 2da. posicion del resultado es nil, devuelve 'Goodbye! (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
(defn repl
  "Inicia el REPL de Scheme."
  ([]
   (println "Interprete de Scheme en Clojure")
   (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2021") (prn)
   (println "Inspirado en:")
   (println "  SCM version 5f2.")                        ; https://people.csail.mit.edu/jaffer/SCM.html
   (println "  Copyright (C) 1990-2006 Free Software Foundation.") (prn) (flush)
   (repl (list 'append 'append 'car 'car 'cdr 'cdr 'cond 'cond 'cons 'cons 'define 'define
               'display 'display 'env 'env 'equal? 'equal? 'eval 'eval 'exit 'exit
               'if 'if 'lambda 'lambda 'length 'length 'list 'list 'list? 'list? 'load 'load
               'newline 'newline 'nil (symbol "#f") 'not 'not 'null? 'null? 'or 'or 'quote 'quote
               'read 'read 'reverse 'reverse 'set! 'set! (symbol "#f") (symbol "#f")
               (symbol "#t") (symbol "#t") '+ '+ '- '- '< '< '> '> '>= '>=)))
  ([amb]
   (print "> ") (flush)
   (try
     (let [renglon (leer-entrada)]                       ; READ
       (if (= renglon "")
         (repl amb)
         (let [str-corregida (proteger-bool-en-str renglon)
               cod-en-str (read-string str-corregida)
               cod-corregido (restaurar-bool cod-en-str)
               res (evaluar cod-corregido amb)]     ; EVAL
           (if (nil? (second res))              ;   Si el ambiente del resultado es `nil`, es porque se ha evaluado (exit)
             'Goodbye!                        ;   En tal caso, sale del REPL devolviendo Goodbye!.
             (do (imprimir (first res))       ; PRINT
                 (repl (second res)))))))     ; LOOP (Se llama a si misma con el nuevo ambiente)
     (catch Exception e                                  ; PRINT (si se lanza una excepcion)
       (imprimir (generar-mensaje-error :error (get (Throwable->map e) :cause)))
       (repl amb)))))                        ; LOOP (Se llama a si misma con el ambiente intacto)


(defn evaluar
  "Evalua una expresion `expre` en un ambiente. Devuelve un lista con un valor resultante y un ambiente."
  [expre amb]
  (if (and (seq? expre) (or (empty? expre) (error? expre))) ; si `expre` es () o error, devolverla intacta
    (list expre amb)                                      ; de lo contrario, evaluarla
    (cond
      (not (seq? expre))             (evaluar-escalar expre amb)

      (igual? (first expre) 'define) (evaluar-define expre amb)

      (igual? (first expre) 'set!) (evaluar-set! expre amb)

      (igual? (first expre) 'if) (evaluar-if expre amb)

      (igual? (first expre) 'or) (evaluar-or expre amb)

      (igual? (first expre) 'exit) (evaluar-exit expre amb)

      (igual? (first expre) 'eval) (evaluar-eval expre amb)

      (igual? (first expre) 'cond) (evaluar-cond expre amb)

      (igual? (first expre) 'quote) (evaluar-quote expre amb)

      (igual? (first expre) 'lambda) (evaluar-lambda expre amb)

      (igual? (first expre) 'load) (evaluar-load expre amb)

      :else (let [res-eval-1 (evaluar (first expre) amb)
                  res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x))] (cons (second res-eval-3) (concat (next x) (list (first res-eval-3)))))) (cons (list (second res-eval-1)) (next expre)))]
              (aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2))))))

(defn aplicar
  "Aplica la funcion `fnc` a la lista de argumentos `lae` evaluados en el ambiente dado."
  ([fnc lae amb]
   (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb))
  ([resu1 resu2 fnc lae amb]
   (cond
     (error? resu1) (list resu1 amb)
     (error? resu2) (list resu2 amb)
     (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb) amb)
     :else (aplicar-lambda fnc lae amb))))

(defn aplicar-lambda
  "Aplica la funcion lambda `fnc` a `lae` (lista de argumentos evaluados)."
  [fnc lae amb]
  (cond
    (not= (count lae) (count (second fnc))) (list (generar-mensaje-error :wrong-number-args fnc) amb)
    (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb)
    :else (aplicar-lambda-multiple fnc lae amb)))

(defn aplicar-lambda-simple
  "Evalua un lambda `fnc` con un cuerpo simple"
  [fnc lae amb]
  (let [lae-con-quotes (map #(if (or (number? %) (string? %) (and (seq? %) (igual? (first %) 'lambda)))
                               %
                               (list 'quote %)) lae)
        nuevos-pares (reduce concat (map list (second fnc) lae-con-quotes))
        mapa (into (hash-map) (vec (map vec (partition 2 nuevos-pares))))
        cuerpo (first (nnext fnc))
        expre (if (and (seq? cuerpo) (seq? (first cuerpo)) (igual? (ffirst cuerpo) 'lambda))
                (cons (first cuerpo) (postwalk-replace mapa (rest cuerpo)))
                (postwalk-replace mapa cuerpo))]
    (evaluar expre amb)))

(defn aplicar-lambda-multiple
  "Evalua una funcion lambda `fnc` cuyo cuerpo contiene varias partes."
  [fnc lae amb]
  (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
           lae
           (second (aplicar-lambda-simple fnc lae amb))))

(defn aplicar-funcion-primitiva
  "Aplica una funcion primitiva a una `lae` (lista de argumentos evaluados)."
  [fnc lae amb]
  (cond
    (= fnc '<)            (fnc-menor lae)

    (= fnc '>)            (fnc-mayor lae)

    (= fnc '>=)            (fnc-mayor-o-igual lae)

    (= fnc '-)            (fnc-restar lae)

    (= fnc '+)            (fnc-sumar lae)

    (igual? fnc 'append)  (fnc-append lae)

    (igual? fnc 'read)  (fnc-read lae)

    (igual? fnc 'car)  (fnc-car lae)

    (igual? fnc 'cdr)  (fnc-cdr lae)

    (igual? fnc 'cons)  (fnc-cons lae)

    (igual? fnc 'display)  (fnc-display lae)

    (igual? fnc 'env)  (fnc-env lae amb)

    (igual? fnc 'length)  (fnc-length lae)

    (igual? fnc 'list)  (fnc-list lae)

    (igual? fnc 'list?)  (fnc-list? lae)

    (igual? fnc 'newline)  (fnc-newline lae)

    (igual? fnc 'null?)  (fnc-null? lae)

    (igual? fnc 'not)  (fnc-not lae)

    (igual? fnc 'reverse)  (fnc-reverse lae)

    (igual? fnc 'equal?)            (fnc-equal? lae)
    
    (igual? fnc 'quote) (evaluar-quote lae amb)

    :else (generar-mensaje-error :wrong-type-apply fnc)))

(defn fnc-car
  "Devuelve el primer elemento de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'car), arg1 (first lae)]
    (cond
      (error? ari) ari
      (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'car arg1)
      :else (first arg1))))

(defn fnc-cdr
  "Devuelve una lista sin su 1ra. posicion."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'cdr), arg1 (first lae)]
    (cond
      (error? ari) ari
      (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'cdr arg1)
      :else (rest arg1))))

(defn fnc-cons
  "Devuelve el resultado de insertar un elemento en la cabeza de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 2 'cons), arg1 (first lae), arg2 (second lae)]
    (cond
      (error? ari) ari
      (not (seq? arg2)) (generar-mensaje-error :only-proper-lists-implemented 'cons)
      :else (cons arg1 arg2))))

(defn fnc-display
  "Imprime un elemento por la termina/consola y devuelve #<unspecified>."
  [lae]
  (let [cant-args (count lae), arg1 (first lae)]
    (case cant-args
      1 (do (print arg1) (flush) (symbol "#<unspecified>"))
      2 (generar-mensaje-error :io-ports-not-implemented 'display)
      (generar-mensaje-error :wrong-number-args-prim-proc 'display))))

(defn fnc-env
  "Devuelve el ambiente."
  [lae amb]
  (let [ari (controlar-aridad-fnc lae 0 'env)]
    (if (error? ari)
      ari
      amb)))

(defn fnc-length
  "Devuelve la longitud de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'length), arg1 (first lae)]
    (cond
      (error? ari) ari
      (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'length arg1)
      :else (count arg1))))

(defn fnc-list
  "Devuelve una lista formada por los args."
  [lae]
  (if (< (count lae) 1)
    ()
    lae))

(defn fnc-list?
  "Devuelve #t si un elemento es una lista. Si no, #f."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'list?), arg1 (first lae)]
    (if (error? ari)
      ari
      (if (seq? arg1)
        (symbol "#t")
        (symbol "#f")))))

(defn fnc-newline
  "Imprime un salto de linea y devuelve #<unspecified>."
  [lae]
  (let [cant-args (count lae)]
    (case cant-args
      0 (do (newline) (flush) (symbol "#<unspecified>"))
      1 (generar-mensaje-error :io-ports-not-implemented 'newline)
      (generar-mensaje-error :wrong-number-args-prim-proc 'newline))))

(defn fnc-not
  "Niega el argumento."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'not)]
    (if (error? ari)
      ari
      (if (igual? (first lae) (symbol "#f"))
        (symbol "#t")
        (symbol "#f")))))

(defn fnc-null?
  "Devuelve #t si un elemento es ()."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'null?)]
    (if (error? ari)
      ari
      (if (= (first lae) ())
        (symbol "#t")
        (symbol "#f")))))

(defn fnc-reverse
  "Devuelve una lista con los elementos de `lae` en orden inverso."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'reverse), arg1 (first lae)]
    (cond
      (error? ari) ari
      (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'reverse arg1)
      :else (reverse arg1))))

(defn controlar-aridad-fnc
  "Si la `lae` tiene la longitud esperada, se devuelve este valor (que es la aridad de la funcion).
   Si no, devuelve una lista con un mensaje de error."
  [lae val-esperado fnc]
  (if (= val-esperado (count lae))
    val-esperado
    (generar-mensaje-error :wrong-number-args-prim-proc fnc)))

(defn imprimir
  "Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas
  con comillas) y devuelve su valor. Muestra errores sin parentesis."
  ([elem]
   (cond
     (= \space elem) elem    ; Si es \space no lo imprime pero si lo devuelve
     (and (seq? elem) (starts-with? (apply str elem) ";")) (imprimir elem elem)
     :else (do (prn elem) (flush) elem)))
  ([lis orig]
   (cond
     (nil? lis) (do (prn) (flush) orig)
     :else (do (pr (first lis))
               (print " ")
               (imprimir (next lis) orig)))))

(defn revisar-fnc
  "Si la `lis` representa un error lo devuelve; si no, devuelve nil."
  [lis] (if (error? lis) lis nil))

(defn revisar-lae
  "Si la `lis` contiene alguna sublista que representa un error lo devuelve; si no, devuelve nil."
  [lis] (first (remove nil? (map revisar-fnc (filter seq? lis)))))

(defn evaluar-cond
  "Evalua una expresion `cond`."
  [expre amb]
  (if (= (count expre) 1) ; si es el operador solo
    (list (generar-mensaje-error :bad-or-missing 'cond expre) amb)
    (let [res (drop-while #(and (seq? %) (not (empty? %))) (next expre))]
      (if (empty? res)
        (evaluar-clausulas-de-cond expre (next expre) amb)
        (list (generar-mensaje-error :bad-or-missing 'cond (first res)) amb)))))

(defn evaluar-clausulas-de-cond
  "Evalua las clausulas de cond."
  [expre lis amb]
  (if (nil? lis)
    (list (symbol "#<unspecified>") amb) ; cuando ninguna fue distinta de #f
    (let [res-eval (if (not (igual? (ffirst lis) 'else))
                     (evaluar (ffirst lis) amb)
                     (if (nil? (next lis))
                       (list (symbol "#t") amb)
                       (list (generar-mensaje-error :bad-else-clause 'cond expre) amb)))]
      (cond
        (error? (first res-eval)) res-eval
        (igual? (first res-eval) (symbol "#f")) (recur expre (next lis) (second res-eval))
        :else (evaluar-secuencia-en-cond (nfirst lis) (second res-eval))))))

(defn evaluar-secuencia-en-cond
  "Evalua secuencialmente las sublistas de `lis`. Devuelve el valor de la ultima evaluacion."
  [lis amb]
  (if (nil? (next lis))
    (evaluar (first lis) amb)
    (let [res-eval (evaluar (first lis) amb)]
      (if (error? (first res-eval))
        res-eval
        (recur (next lis) (second res-eval))))))

(defn evaluar-eval
  "Evalua una expresion `eval`."
  [expre amb]
  (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
    (list (generar-mensaje-error :wrong-number-args (symbol "#<CLOSURE <anon> ...")) amb)
    (let [arg (second expre)]
      (if (and (seq? arg) (igual? (first arg) 'quote))
        (evaluar (second arg) amb)
        (evaluar arg amb)))))

(defn evaluar-exit
  "Sale del interprete de Scheme."
  [expre amb]
  (if (> (count expre) 2) ; si son el operador y mas de 1 argumento
    (list (generar-mensaje-error :wrong-number-args-prim-proc 'quit) amb)
    (list nil nil)))

(defn evaluar-lambda
  "Evalua una expresion `lambda`."
  [expre amb]
  (cond
    (< (count expre) 3) ; si son el operador solo o con 1 unico argumento
    (list (generar-mensaje-error :bad-body 'lambda (rest expre)) amb)
    (not (seq? (second expre)))
    (list (generar-mensaje-error :bad-params 'lambda expre) amb)
    :else (list expre amb)))

(defn evaluar-load
  "Evalua una expresion `load`. Carga en el ambiente un archivo `expre` de codigo en Scheme."
  [expre amb]
  (if (= (count expre) 1) ; si es el operador solo
    (list (generar-mensaje-error :wrong-number-args (symbol "#<CLOSURE scm:load ...")) amb)
    (list (symbol "#<unspecified>") (cargar-arch amb (second expre)))))

(defn cargar-arch
  "Carga y devuelve el contenido de un archivo."
  ([amb arch]
   (let [res (evaluar arch amb)
         nom-original (first res)
         nuevo-amb (second res)]
     (if (error? nom-original)
       (do (imprimir nom-original) nuevo-amb)                 ; Mostrar el error
       (let [nom-a-usar (generar-nombre-arch nom-original)]
         (if (error? nom-a-usar)
           (do (imprimir nom-a-usar) nuevo-amb)          ; Mostrar el error
           (let [tmp (try
                       (slurp nom-a-usar)
                       (catch java.io.FileNotFoundException _
                         (generar-mensaje-error :file-not-found)))]
             (if (error? tmp)
               (do (imprimir tmp) nuevo-amb)        ; Mostrar el error
               (do (spit "scm-temp" (proteger-bool-en-str tmp))
                   (let [ret (with-open [in (java.io.PushbackReader. (reader "scm-temp"))]
                               (binding [*read-eval* false]
                                 (try
                                   (imprimir (list (symbol ";loading") (symbol nom-original)))
                                   (cargar-arch (second (evaluar (restaurar-bool (read in)) nuevo-amb)) in nom-original nom-a-usar)
                                   (catch Exception e
                                     (imprimir (generar-mensaje-error :end-of-file 'list))))))]
                     (do (delete-file "scm-temp" true) ret))))))))))
  ([amb in nom-orig nom-usado]
   (try
     (cargar-arch (second (evaluar (restaurar-bool (read in)) amb)) in nom-orig nom-usado)
     (catch Exception _
       (imprimir (list (symbol ";done loading") (symbol nom-usado)))
       amb))))

(defn generar-nombre-arch
  "Dada una entrada la convierte en un nombre de archivo .scm valido."
  [nom]
  (if (not (string? nom))
    (generar-mensaje-error :wrong-type-arg1 'string-length nom)
    (let [n (lower-case nom)]
      (if (nombre-arch-valido? n)
        n
        (str n ".scm")))))    ; Agrega '.scm' al final


(defn nombre-arch-valido?
  "Chequea que el string sea un nombre de archivo .scm valido."
  [nombre] (and (> (count nombre) 4) (ends-with? nombre ".scm")))

(defn evaluar-quote
  "Evalua una expresion `quote`."
  [expre amb]
  (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
    (list (generar-mensaje-error :missing-or-extra 'quote expre) amb)
    (list (second expre) amb)))

(defn generar-mensaje-error
  "Devuelve un mensaje de error expresado como lista."
  ([cod]
   (case cod
     :file-not-found (list (symbol ";ERROR:") 'No 'such 'file 'or 'directory)
     :warning-paren (list (symbol ";WARNING:") 'unexpected (symbol "\")\"#<input-port 0>"))
     ()))
  ([cod fnc]
   (cons (symbol ";ERROR:")
         (case cod
           :end-of-file (list (symbol (str fnc ":")) 'end 'of 'file)
           :error (list (symbol (str fnc)))
           :io-ports-not-implemented (list (symbol (str fnc ":")) 'Use 'of 'I/O 'ports 'not 'implemented)
           :only-proper-lists-implemented (list (symbol (str fnc ":")) 'Only 'proper 'lists 'are 'implemented)
           :unbound-variable (list 'unbound (symbol "variable:") fnc)
           :wrong-number-args (list 'Wrong 'number 'of 'args 'given fnc)
           :wrong-number-args-oper (list (symbol (str fnc ":")) 'Wrong 'number 'of 'args 'given)
           :wrong-number-args-prim-proc (list 'Wrong 'number 'of 'args 'given (symbol "#<primitive-procedure") (symbol (str fnc '>)))
           :wrong-type-apply (list 'Wrong 'type 'to 'apply fnc)
           ())))
  ([cod fnc nom-arg]
   (cons (symbol ";ERROR:") (cons (symbol (str fnc ":"))
                                  (case cod
                                    :bad-body (list 'bad 'body nom-arg)
                                    :bad-else-clause (list 'bad 'ELSE 'clause nom-arg)
                                    :bad-or-missing (list 'bad 'or 'missing 'clauses nom-arg)
                                    :bad-params (list 'Parameters 'are 'implemented 'only 'as 'lists nom-arg)
                                    :bad-variable (list 'bad 'variable nom-arg)
                                    :missing-or-extra (list 'missing 'or 'extra 'expression nom-arg)
                                    :wrong-type-arg (list 'Wrong 'type 'in 'arg nom-arg)
                                    :wrong-type-arg1 (list 'Wrong 'type 'in 'arg1 nom-arg)
                                    :wrong-type-arg2 (list 'Wrong 'type 'in 'arg2 nom-arg)
                                    ())))))


; FUNCIONES QUE DEBEN SER IMPLEMENTADAS PARA COMPLETAR EL INTERPRETE DE SCHEME (ADEMAS DE COMPLETAR `EVALUAR` Y `APLICAR-FUNCION-PRIMITIVA`):

; LEER-ENTRADA:
; user=> (leer-entrada)
; (hola
; mundo)
; "(hola mundo)"
; user=> (leer-entrada)
; 123
; "123"


(defn leer-entrada
  "Lee una cadena desde la terminal/consola. Si los parentesis no estan correctamente balanceados al presionar Enter/Intro,
   se considera que la cadena ingresada es una subcadena y el ingreso continua. De lo contrario, se la devuelve completa."
  []
  (loop [input (read-line) acc []]
    (if (= (verificar-parentesis (apply str (conj acc input))) 0)
      (apply str (conj acc input))
      (recur (read-line) (conj acc input " ")))))


; user=> (verificar-parentesis "(hola 'mundo")
; 1
; user=> (verificar-parentesis "(hola '(mundo)))")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) )")
; 0


(defn verificar-parentesis
  "Cuenta los parentesis en una cadena, sumando 1 si `(`, restando 1 si `)`. Si el contador se hace negativo, para y retorna -1."
  [x]
  (reduce (fn [sum c]
            (if (< sum 0)
              (reduced -1)
              (case c
                \( (+ sum 1)
                \) (- sum 1)
                sum)))
          0
          x))

(defn indice [x amb]
  (+ (first (keep-indexed #(if (and (even? %1) (igual? x %2)) %1) amb)) 1))

; user=> (actualizar-amb '(a 1 b 2 c 3) 'd 4)
; (a 1 b 2 c 3 d 4)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b 4)
; (a 1 b 4 c 3)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))
; (a 1 b 2 c 3)
; user=> (actualizar-amb () 'b 7)
; (b 7)
(defn actualizar-amb
  "Devuelve un ambiente actualizado con una clave (nombre de la variable o funcion) y su valor. 
  Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza la nueva informacion."
  [amb key value]
  (cond
    (error? value) amb
    (error? (buscar key amb)) (concat amb (list key value))
    :else (let [key_index (indice key amb)]
            (map-indexed (fn [idx itm] (if (= idx key_index) value itm)) amb))))



; user=> (buscar 'c '(a 1 b 2 c 3 d 4 e 5))
; 3
; user=> (buscar 'f '(a 1 b 2 c 3 d 4 e 5))
; (;ERROR: unbound variable: f)


(defn buscar
  "Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
   y devuelve el valor asociado. Devuelve un error :unbound-variable si no la encuentra."
  [x amb]
  (try
    (nth amb (indice x amb))
    (catch Exception e (generar-mensaje-error :unbound-variable x))))

; user=> (error? (list (symbol ";ERROR:") 'mal 'hecho))
; true
; user=> (error? (list 'mal 'hecho))
; false
; user=> (error? (list (symbol ";WARNING:") 'mal 'hecho))
; true
(defn error?
  "Devuelve true o false, segun sea o no el arg. una lista con `;ERROR:` o `;WARNING:` como primer elemento."
  [args]
  (and (seq? args) (or (= (first args) (symbol ";ERROR:")) (= (first args) (symbol ";WARNING:")))))

; user=> (proteger-bool-en-str "(or #F #f #t #T)")
; "(or %F %f %t %T)"
; user=> (proteger-bool-en-str "(and (or #F #f #t #T) #T)")
; "(and (or %F %f %t %T) %T)"
; user=> (proteger-bool-en-str "")
; ""
(defn proteger-bool-en-str
  "Cambia, en una cadena, #t por %t y #f por %f (y sus respectivas versiones en mayusculas), para poder aplicarle read-string."
  [string]
  (st/replace string #"#" "%"))

; user=> (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
; (and (or #F #f #t #T) #T)
; user=> (restaurar-bool (read-string "(and (or %F %f %t %T) %T)") )
; (and (or #F #f #t #T) #T)
(defn restaurar-bool
  "Cambia, en un codigo leido con read-string, %t por #t y %f por #f (y sus respectivas versiones en mayusculas)."
  [sentence]
  (map (fn [item] (if (seq? item) (restaurar-bool item) (if (symbol? item) (symbol (st/replace item #"%" "#")) item))) sentence))


(defn array-equal?
  [elements]
  (if (empty? elements) true (reduce (fn [result c] (cond
                                                          (nil? (first result)) (reduced true)
                                                          (igual? (first result) c) (rest result)
                                                          :else (reduced false)))
                                         (rest elements) elements)))

; user=> (igual? 'if 'IF)
; true
; user=> (igual? 'if 'if)
; true
; user=> (igual? 'IF 'IF)
; true
; user=> (igual? 'IF "IF")
; false
; user=> (igual? 6 "6")
; false
(defn igual?
  "Verifica la igualdad entre dos elementos al estilo de Scheme (case-insensitive)"
  [a b]
  (cond
    (and (int? a) (int? b))(= a b)
    (and (seq? a) (seq? b))(= a b)
    (= (type a) (type b)) (= (st/lower-case a) (st/lower-case b))
    :else false))

; user=> (fnc-append '( (1 2) (3) (4 5) (6 7)))
; (1 2 3 4 5 6 7)
; user=> (fnc-append '( (1 2) 3 (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg 3)
; user=> (fnc-append '( (1 2) A (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg A)
(defn fnc-append
  "Devuelve el resultado de fusionar listas."
  [lists]
  (reduce (fn [result c] (if (seq? c)
                           (concat result c)
                           (reduced (generar-mensaje-error :wrong-type-arg 'append c))))
          '() lists))

(defn traduce-bool [bool] (cond
                            (seq? bool) bool ; is and error
                            :else (if bool (symbol "#t") (symbol "#f"))))

(defn inverse-traduce-bool [scheme-bool] (cond
                                           (seq? scheme-bool) scheme-bool ; is an error
                                           (number? scheme-bool) scheme-bool
                                           (= (symbol "#t") scheme-bool) true
                                           :else false))

; user=> (fnc-equal? ())
; #t
; user=> (fnc-equal? '(A))
; #t
; user=> (fnc-equal? '(A a))
; #t
; user=> (fnc-equal? '(A a A))
; #t
; user=> (fnc-equal? '(A a A a))
; #t
; user=> (fnc-equal? '(A a A B))
; #f
; user=> (fnc-equal? '(1 1 1 1))
; #t
; user=> (fnc-equal? '(1 1 2 1))
; #f
(defn fnc-equal?
  "Compara elementos. Si son iguales, devuelve #t. Si no, #f."
  [elements]
  (traduce-bool (array-equal? elements)))

; user=> (fnc-read ())
; (hola
; mundo)
; (hola mundo)
; user=> (fnc-read '(1))
; (;ERROR: read: Use of I/O ports not implemented)
; user=> (fnc-read '(1 2))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
; user=> (fnc-read '(1 2 3))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
(defn fnc-read
  "Devuelve la lectura de un elemento de Scheme desde la terminal/consola."
  [args]
  (cond
    (empty? args) (read-string (leer-entrada)) 
    (= 1 (count args)) (generar-mensaje-error :io-ports-not-implemented "read")
    :else (generar-mensaje-error :wrong-number-args-prim-proc "read")))

; user=> (fnc-sumar ())
; 0
; user=> (fnc-sumar '(3))
; 3
; user=> (fnc-sumar '(3 4))
; 7
; user=> (fnc-sumar '(3 4 5))
; 12
; user=> (fnc-sumar '(3 4 5 6))
; 18
; user=> (fnc-sumar '(A 4 5 6))
; (;ERROR: +: Wrong type in arg1 A)
; user=> (fnc-sumar '(3 A 5 6))
; (;ERROR: +: Wrong type in arg2 A)
; user=> (fnc-sumar '(3 4 A 6))
; (;ERROR: +: Wrong type in arg2 A)
(defn fnc-sumar
  "Suma los elementos de una lista."
  [elements]
  (cond
    (empty? elements) 0
    (not (integer? (first elements))) (generar-mensaje-error :wrong-type-arg1 "+" (first elements))
    :else (reduce (fn [result c] (try (+ c result) (catch Exception e (reduced (generar-mensaje-error :wrong-type-arg2 "+" c)))))
                  0 elements)))

; user=> (fnc-restar ())
; (;ERROR: -: Wrong number of args given)
; user=> (fnc-restar '(3))
; -3
; user=> (fnc-restar '(3 4))
; -1
; user=> (fnc-restar '(3 4 5))
; -6
; user=> (fnc-restar '(3 4 5 6))
; -12
; user=> (fnc-restar '(A 4 5 6))
; (;ERROR: -: Wrong type in arg1 A)
; user=> (fnc-restar '(3 A 5 6))
; (;ERROR: -: Wrong type in arg2 A)
; user=> (fnc-restar '(3 4 A 6))
; (;ERROR: -: Wrong type in arg2 A)
(defn fnc-restar
  "Resta los elementos de un lista."
  [elements]
  (cond
    (empty? elements) (generar-mensaje-error :wrong-number-args-oper "-")
    (= 1 (count elements)) (- 0 (first elements))
    (not (integer? (first elements))) (generar-mensaje-error :wrong-type-arg1 "-" (first elements))
    :else (reduce (fn [result c] (try (- result c) (catch Exception e (reduced (generar-mensaje-error :wrong-type-arg2 "-" c)))))
                  (* 2 (first elements)) elements)))

(defn fnc-comp
  [elements op opname]
  (traduce-bool (cond
                  (empty? elements) true
                  (not (integer? (first elements))) (generar-mensaje-error :wrong-type-arg1 opname (first elements))
                  :else (reduce (fn [result c] (cond
                                                 (nil? (first result)) (reduced true)
                                                 (not (integer? c)) (reduced (generar-mensaje-error :wrong-type-arg2 opname c))
                                                 (not (integer? (first result))) (reduced (generar-mensaje-error :wrong-type-arg2 opname (first result)))
                                                 (op c (first result)) (rest result)
                                                 :else (reduced false)))
                                (rest elements) elements))))

; user=> (fnc-menor ())
; #t
; user=> (fnc-menor '(1))
; #t
; user=> (fnc-menor '(1 2))
; #t
; user=> (fnc-menor '(1 2 3))
; #t
; user=> (fnc-menor '(1 2 3 4))
; #t
; user=> (fnc-menor '(1 2 2 4))
; #f
; user=> (fnc-menor '(1 2 1 4))
; #f
; user=> (fnc-menor '(A 1 2 4))
; (;ERROR: <: Wrong type in arg1 A)
; user=> (fnc-menor '(1 A 1 4))
; (;ERROR: <: Wrong type in arg2 A)
; user=> (fnc-menor '(1 2 A 4))
; (;ERROR: <: Wrong type in arg2 A)
(defn fnc-menor
  "Devuelve #t si los numeros de una lista estan en orden estrictamente creciente; si no, #f."
  [elements]
  (fnc-comp elements < "<"))

; user=> (fnc-mayor ())
; #t
; user=> (fnc-mayor '(1))
; #t
; user=> (fnc-mayor '(2 1))
; #t
; user=> (fnc-mayor '(3 2 1))
; #t
; user=> (fnc-mayor '(4 3 2 1))
; #t
; user=> (fnc-mayor '(4 2 2 1))
; #f
; user=> (fnc-mayor '(4 2 1 4))
; #f
; user=> (fnc-mayor '(A 3 2 1))
; (;ERROR: >: Wrong type in arg1 A)
; user=> (fnc-mayor '(3 A 2 1))
; (;ERROR: >: Wrong type in arg2 A)
; user=> (fnc-mayor '(3 2 A 1))
; (;ERROR: >: Wrong type in arg2 A)
(defn fnc-mayor
  "Devuelve #t si los numeros de una lista estan en orden estrictamente decreciente; si no, #f."
  [elements]
  (fnc-comp elements > ">"))

; user=> (fnc-mayor-o-igual ())
; #t
; user=> (fnc-mayor-o-igual '(1))
; #t
; user=> (fnc-mayor-o-igual '(2 1))
; #t
; user=> (fnc-mayor-o-igual '(3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 1 4))
; #f
; user=> (fnc-mayor-o-igual '(A 3 2 1))
; (;ERROR: >=: Wrong type in arg1 A)
; user=> (fnc-mayor-o-igual '(3 A 2 1))
; (;ERROR: >=: Wrong type in arg2 A)
; user=> (fnc-mayor-o-igual '(3 2 A 1))
; (;ERROR: >=: Wrong type in arg2 A)
(defn fnc-mayor-o-igual
  "Devuelve #t si los numeros de una lista estan en orden decreciente; si no, #f."
  [elements]
  (fnc-comp elements >= ">="))

; user=> (evaluar-escalar 32 '(x 6 y 11 z "hola"))
; (32 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar "chau" '(x 6 y 11 z "hola"))
; ("chau" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'y '(x 6 y 11 z "hola"))
; (11 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'z '(x 6 y 11 z "hola"))
; ("hola" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'n '(x 6 y 11 z "hola"))
; ((;ERROR: unbound variable: n) (x 6 y 11 z "hola"))
(defn evaluar-escalar
  "Evalua una expresion escalar. Devuelve una lista con el resultado y un ambiente."
  [var amb]
  (list (cond
          (integer? var) var
          (string? var) var
          (= (symbol "#<unspecified>") var) var
          :else (buscar var amb)) amb))

(defn definir-funcion
  [func amb]
  (concat amb (list (first (first func)) (concat (list 'lambda (rest (first func))) (rest func)))))

(defn m-e-error
  [func expre amb] (list (generar-mensaje-error :missing-or-extra func expre) amb))

; user=> (evaluar-define '(define x 2) '(x 1))
; (#<unspecified> (x 2))
; user=> (evaluar-define '(define (f x) (+ x 1)) '(x 1))
; (#<unspecified> (x 1 f (lambda (x) (+ x 1))))
; user=> (evaluar-define '(define) '(x 1))
; ((;ERROR: define: missing or extra expression (define)) (x 1))
; user=> (evaluar-define '(define x) '(x 1))
; ((;ERROR: define: missing or extra expression (define x)) (x 1))
; user=> (evaluar-define '(define x 2 3) '(x 1))
; ((;ERROR: define: missing or extra expression (define x 2 3)) (x 1))
; user=> (evaluar-define '(define ()) '(x 1))
; ((;ERROR: define: missing or extra expression (define ())) (x 1))
; user=> (evaluar-define '(define () 2) '(x 1))
; ((;ERROR: define: bad variable (define () 2)) (x 1))
; user=> (evaluar-define '(define 2 x) '(x 1))
; ((;ERROR: define: bad variable (define 2 x)) (x 1))
; user=> (evaluar-define '(define (f x) (display x) (newline) (+ x 1)) '(x 1))
; (#<unspecified> (x 1 f (lambda (x) (display x) (newline) (+ x 1))))
(defn evaluar-define
  "Evalua una expresion `define`. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."
  [expre amb]
  (cond
    (>= (count expre) 3) (cond
                           (symbol? (second expre)) (if (> (count expre) 3) (m-e-error "define" expre amb) (list (symbol "#<unspecified>") (actualizar-amb amb (second expre) (first (evaluar (last expre) amb)))))
                           (and (seq? (second expre)) (> (count (second expre)) 0)) (list (symbol "#<unspecified>") (definir-funcion (rest expre) amb))
                           :else (list (generar-mensaje-error :bad-variable "define" expre) amb))
    :else (m-e-error "define" expre amb)))

; user=> (evaluar-if '(if 1 2) '(n 7))
; (2 (n 7))
; user=> (evaluar-if '(if 1 n) '(n 7))
; (7 (n 7))
; user=> (evaluar-if '(if 1 n 8) '(n 7))
; (7 (n 7))
; user=> (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))
; (8 (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 9 #f #f))
; user=> (evaluar-if '(if) '(n 7))
; ((;ERROR: if: missing or extra expression (if)) (n 7))
; user=> (evaluar-if '(if 1) '(n 7))
; ((;ERROR: if: missing or extra expression (if 1)) (n 7))
(defn evaluar-if
  "Evalua una expresion `if`. Devuelve una lista con el resultado y un ambiente eventualmente modificado."
  [expre amb]
  (let [n (count expre)] (cond
                           (< n 3) (m-e-error "if" expre amb)
                           (> n 4) (m-e-error "if" expre amb)
                           (= n 3) (if (inverse-traduce-bool (first (evaluar (second expre) amb))) (evaluar (second (next expre)) amb) (list (symbol "#<unspecified>") amb))
                           (= n 4) (evaluar (if (inverse-traduce-bool (first (evaluar (second expre) amb))) (second (next expre)) (last expre)) amb))))

; user=> (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#t (#f #f #t #t))
; user=> (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (7 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (5 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
(defn evaluar-or
  "Evalua una expresion `or`.  Devuelve una lista con el resultado y un ambiente."
  [expre amb]

  (let [args (rest expre)] (if (empty? args) (list (symbol "#f") amb) (list (reduce (fn [result c] (let [evaluated (first (evaluar c amb))] (cond
                                                          (not (= (symbol "#f") evaluated)) (reduced evaluated)
                                                          :else result)))
        (symbol "#f") args) amb))))

; user=> (evaluar-set! '(set! x 1) '(x 0))
; (#<unspecified> (x 1))
; user=> (evaluar-set! '(set! x 1) '())
; ((;ERROR: unbound variable: x) ())
; user=> (evaluar-set! '(set! x) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x)) (x 0))
; user=> (evaluar-set! '(set! x 1 2) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x 1 2)) (x 0))
; user=> (evaluar-set! '(set! 1 2) '(x 0))
; ((;ERROR: set!: bad variable 1) (x 0))
(defn evaluar-set!
  "Evalua una expresion `set!`. Devuelve una lista con el resultado y un ambiente actualizado con la redefinicion."
  [expre amb]
  (cond
    (= (count expre) 3) (cond
                          (not (symbol? (second expre))) (list (generar-mensaje-error :bad-variable "set!" (second expre)) amb)
                          (error? (buscar (second expre) amb)) (list (generar-mensaje-error :unbound-variable (second expre)) amb)
                          :else (list (symbol "#<unspecified>") (actualizar-amb amb (second expre) (first (evaluar (last expre) amb)))))
    :else (list (generar-mensaje-error :missing-or-extra "set!" expre) amb)))


; Al terminar de cargar el archivo en el REPL de Clojure, se debe devolver true.


(defn -main
  "Funcion ppal del interprete de Scheme"
  [& args]
  (repl))
