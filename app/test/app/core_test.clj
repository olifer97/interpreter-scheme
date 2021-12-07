(ns app.core-test
  (:require [clojure.test :refer :all]
            [app.core :refer :all]))

; verificar-parentesis
(deftest verificar-parentesis-0-test
  (testing "verificar-parentesis"
    (is (= (verificar-parentesis "(hola 'mundo") 1))))

(deftest verificar-parentesis-1-test
  (testing "verificar-parentesis"
    (is (= (verificar-parentesis "(hola '(mundo)))") -1))))

(deftest verificar-parentesis-2-test
  (testing "verificar-parentesis"
    (is (= (verificar-parentesis "(hola '(mundo) () 6) 7)") -1))))

(deftest verificar-parentesis-3-test
  (testing "verificar-parentesis"
    (is (= (verificar-parentesis "(hola '(mundo) () 6) 7) 9)") -1))))

(deftest verificar-parentesis-3-test
  (testing "verificar-parentesis"
    (is (= (verificar-parentesis "(hola '(mundo) )") 0))))

; buscar

(deftest buscar-0-test
  (testing "buscar"
    (is (= (buscar 'c '(a 1 b 2 c 3 d 4 e 5)) 3))))

(deftest buscar-1-test
  (testing "buscar"
    (is (= (buscar 'f '(a 1 b 2 c 3 d 4 e 5)) (list (symbol ";ERROR:") 'unbound (symbol "variable:") 'f)))))

; error?

(deftest error-0-test
  (testing "error"
    (is (= (error? (list (symbol ";ERROR:") 'mal 'hecho)) true))))

(deftest error-1-test
  (testing "error"
    (is (= (error? (list 'mal 'hecho)) false))))

(deftest error-1-test
  (testing "error"
    (is (= (error? (list (symbol ";WARNING:") 'mal 'hecho)) true))))

; actualizar-amb

(deftest actualizar-amb-0-test
  (testing "actualizar-amb"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'd 4) '(a 1 b 2 c 3 d 4)))))

(deftest actualizar-amb-1-test
  (testing "actualizar-amb"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b 4) '(a 1 b 4 c 3)))))

(deftest actualizar-amb-2-test
  (testing "actualizar-amb"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho)) '(a 1 b 2 c 3)))))

(deftest actualizar-amb-3-test
  (testing "actualizar-amb"
    (is (= (actualizar-amb () 'b 7) '(b 7)))))

; proteger-bool-en-str

(deftest proteger-bool-en-str-0-test
  (testing "proteger-bool-en-str"
    (is (= (proteger-bool-en-str "(or #F #f #t #T)") "(or %F %f %t %T)"))))

(deftest proteger-bool-en-str-1-test
  (testing "proteger-bool-en-str"
    (is (= (proteger-bool-en-str "(and (or #F #f #t #T) #T)") "(and (or %F %f %t %T) %T)"))))

(deftest proteger-bool-en-str-2-test
  (testing "proteger-bool-en-str"
    (is (= (proteger-bool-en-str "") ""))))

; restaurar-bool

(deftest restaurar-bool-0-test
  (testing "restaurar-bool"
    (is (= (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)"))) (list 'and (list 'or (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))))))

(deftest restaurar-bool-1-test
  (testing "restaurar-bool"
    (is (= (restaurar-bool (read-string "(and (or %F %f %t %T) %T)")) (list 'and (list 'or (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))))))

; igual?

(deftest igual?-0-test
  (testing "igual?"
    (is (= (igual? 'if 'IF) true))))


(deftest igual?-1-test
  (testing "igual?"
    (is (= (igual? 'if 'if) true))))

(deftest igual?-2-test
  (testing "igual?"
    (is (= (igual? 'IF 'IF) true))))

(deftest igual?-3-test
  (testing "igual?"
    (is (= (igual? 'IF "IF") false))))

(deftest igual?-4-test
  (testing "igual?"
    (is (= (igual? 6 "6") false))))

;fnc-append

(deftest fnc-append-0-test
  (testing "fnc-append"
    (is (= (fnc-append '((1 2) (3) (4 5) (6 7))) '(1 2 3 4 5 6 7)))))

(deftest fnc-append-1-test
  (testing "fnc-append"
    (is (= (fnc-append '((1 2) 3 (4 5) (6 7))) (list (symbol ";ERROR:") (symbol "append:") 'Wrong 'type 'in 'arg '3)))))

(deftest fnc-append-2-test
  (testing "fnc-append"
    (is (= (fnc-append '((1 2) A (4 5) (6 7))) (list (symbol ";ERROR:") (symbol "append:") 'Wrong 'type 'in 'arg 'A)))))

;fnc-equal?

(deftest fnc-equal?-0-test
  (testing "fnc-equal?"
    (is (= (fnc-equal? '()) (symbol "#t")))))

(deftest fnc-equal?-1-test
  (testing "fnc-equal?"
    (is (= (fnc-equal? '(A)) (symbol "#t")))))

(deftest fnc-equal?-2-test
  (testing "fnc-equal?"
    (is (= (fnc-equal? '(A a)) (symbol "#t")))))

(deftest fnc-equal?-3-test
  (testing "fnc-equal?"
    (is (= (fnc-equal? '(A a A)) (symbol "#t")))))

(deftest fnc-equal?-4-test
  (testing "fnc-equal?"
    (is (= (fnc-equal? '(A a A a)) (symbol "#t")))))

(deftest fnc-equal?-5-test
  (testing "fnc-equal?"
    (is (= (fnc-equal? '(A a A B)) (symbol "#f")))))

(deftest fnc-equal?-6-test
  (testing "fnc-equal?"
    (is (= (fnc-equal? '(1 1 1 1)) (symbol "#t")))))

(deftest fnc-equal?-7-test
  (testing "fnc-equal?"
    (is (= (fnc-equal? '(1 1 2 1)) (symbol "#f")))))

;fnc-sumar

(deftest fnc-sumar-0-test
  (testing "fnc-sumar"
    (is (= (fnc-sumar ()) 0))))

(deftest fnc-sumar-1-test
  (testing "fnc-sumar"
    (is (= (fnc-sumar '(3)) 3))))

(deftest fnc-sumar-2-test
  (testing "fnc-sumar"
    (is (= (fnc-sumar '(3 4)) 7))))

(deftest fnc-sumar-3-test
  (testing "fnc-sumar"
    (is (= (fnc-sumar '(3 4 5)) 12))))

(deftest fnc-sumar-4-test
  (testing "fnc-sumar"
    (is (= (fnc-sumar '(A 4 5 6)) (list (symbol ";ERROR:") (symbol "+:") 'Wrong 'type 'in 'arg1 'A)))))

(deftest fnc-sumar-5-test
  (testing "fnc-sumar"
    (is (= (fnc-sumar '(4 A 5 6)) (list (symbol ";ERROR:") (symbol "+:") 'Wrong 'type 'in 'arg2 'A)))))

; fnc-restar

(deftest fnc-restar-0-test
  (testing "fnc-restar"
    (is (= (fnc-restar ()) (list (symbol ";ERROR:") (symbol "-:") 'Wrong 'number 'of 'args 'given)))))

(deftest fnc-restar-1-test
  (testing "fnc-restar"
    (is (= (fnc-restar '(3)) -3))))

(deftest fnc-restar-2-test
  (testing "fnc-restar"
    (is (= (fnc-restar '(3 4)) -1))))

(deftest fnc-restar-3-test
  (testing "fnc-restar"
    (is (= (fnc-restar '(3 4 5)) -6))))

(deftest fnc-restar-4-test
  (testing "fnc-restar"
    (is (= (fnc-restar '(A 4 5 6)) (list (symbol ";ERROR:") (symbol "-:") 'Wrong 'type 'in 'arg1 'A)))))

(deftest fnc-restar-5-test ; TODO: check
  (testing "fnc-restar"
    (is (= (fnc-restar '(4 A 5 6)) (list (symbol ";ERROR:") (symbol "-:") 'Wrong 'type 'in 'arg2 'A)))))

; fnc-menor
(deftest fnc-menor-0-test
  (testing "fnc-menor"
    (is (= (fnc-menor ()) (symbol "#t")))))

(deftest fnc-menor-1-test
  (testing "fnc-menor"
    (is (= (fnc-menor '(1)) (symbol "#t")))))

(deftest fnc-menor-2-test
  (testing "fnc-menor"
    (is (= (fnc-menor '(1 2)) (symbol "#t")))))

(deftest fnc-menor-3-test
  (testing "fnc-menor"
    (is (= (fnc-menor '(1 2 3)) (symbol "#t")))))

(deftest fnc-menor-4-test
  (testing "fnc-menor"
    (is (= (fnc-menor '(1 2 2 4)) (symbol "#f")))))

(deftest fnc-menor-5-test
  (testing "fnc-menor"
    (is (= (fnc-menor '(1 2 1 4)) (symbol "#f")))))

(deftest fnc-menor-6-test
  (testing "fnc-menor"
    (is (= (fnc-menor '(A 1 2 4)) (list (symbol ";ERROR:") (symbol "<:") 'Wrong 'type 'in 'arg1 'A)))))

(deftest fnc-menor-7-test
  (testing "fnc-menor"
    (is (= (fnc-menor '(1 A 1 4)) (list (symbol ";ERROR:") (symbol "<:") 'Wrong 'type 'in 'arg2 'A)))))

(deftest fnc-menor-8-test
  (testing "fnc-menor"
    (is (= (fnc-menor '(1 2 A 4)) (list (symbol ";ERROR:") (symbol "<:") 'Wrong 'type 'in 'arg2 'A)))))


; fnc-mayor-o-igual
(deftest fnc-mayor-0-test
  (testing "fnc-mayor"
    (is (= (fnc-mayor ()) (symbol "#t")))))

(deftest fnc-mayor-1-test
  (testing "fnc-mayor"
    (is (= (fnc-mayor '(1)) (symbol "#t")))))

(deftest fnc-mayor-2-test
  (testing "fnc-mayor"
    (is (= (fnc-mayor '(2 1)) (symbol "#t")))))

(deftest fnc-mayor-3-test
  (testing "fnc-mayor"
    (is (= (fnc-mayor '(3 2 1)) (symbol "#t")))))

(deftest fnc-mayor-4-test
  (testing "fnc-mayor"
    (is (= (fnc-mayor '(4 2 2 1)) (symbol "#f")))))

(deftest fnc-mayor-5-test
  (testing "fnc-mayor"
    (is (= (fnc-mayor '(2 1 4 1)) (symbol "#f")))))

(deftest fnc-mayor-6-test
  (testing "fnc-mayor"
    (is (= (fnc-mayor '(A 4 2 1)) (list (symbol ";ERROR:") (symbol ">:") 'Wrong 'type 'in 'arg1 'A)))))

(deftest fnc-mayor-7-test
  (testing "fnc-mayor"
    (is (= (fnc-mayor '(4 A 3 2)) (list (symbol ";ERROR:") (symbol ">:") 'Wrong 'type 'in 'arg2 'A)))))

(deftest fnc-mayor-8-test
  (testing "fnc-mayor"
    (is (= (fnc-mayor '(2 1 A 0)) (list (symbol ";ERROR:") (symbol ">:") 'Wrong 'type 'in 'arg2 'A)))))

; fnc-mayor-o-igual
(deftest fnc-mayor-o-igual-0-test
  (testing "fnc-mayor-o-igual"
    (is (= (fnc-mayor-o-igual ()) (symbol "#t")))))

(deftest fnc-mayor-o-igual-1-test
  (testing "fnc-mayor-o-igual"
    (is (= (fnc-mayor-o-igual '(1)) (symbol "#t")))))

(deftest fnc-mayor-o-igual-2-test
  (testing "fnc-mayor-o-igual"
    (is (= (fnc-mayor-o-igual '(2 1)) (symbol "#t")))))

(deftest fnc-mayor-o-igual-3-test
  (testing "fnc-mayor-o-igual"
    (is (= (fnc-mayor-o-igual '(3 2 1)) (symbol "#t")))))

(deftest fnc-mayor-o-igual-4-test
  (testing "fnc-mayor-o-igual"
    (is (= (fnc-mayor-o-igual '(4 2 2 1)) (symbol "#t")))))

(deftest fnc-mayor-o-igual-5-test
  (testing "fnc-mayor-o-igual"
    (is (= (fnc-mayor-o-igual '(2 1 4 1)) (symbol "#f")))))

(deftest fnc-mayor-o-igual-6-test
  (testing "fnc-mayor-o-igual"
    (is (= (fnc-mayor-o-igual '(A 4 2 1)) (list (symbol ";ERROR:") (symbol ">=:") 'Wrong 'type 'in 'arg1 'A)))))

(deftest fnc-mayor-o-igual-7-test
  (testing "fnc-mayor-o-igual"
    (is (= (fnc-mayor-o-igual '(4 A 3 2)) (list (symbol ";ERROR:") (symbol ">=:") 'Wrong 'type 'in 'arg2 'A)))))

(deftest fnc-mayor-o-igual-8-test
  (testing "fnc-mayor-o-igual"
    (is (= (fnc-mayor-o-igual '(2 1 A 0)) (list (symbol ";ERROR:") (symbol ">=:") 'Wrong 'type 'in 'arg2 'A)))))

; evaluar-escalar

(deftest evaluar-escalar-0-test
  (testing "evaluar-escalar"
    (is (= (evaluar-escalar 32 '(x 6 y 11 z "hola")) (list 32 '(x 6 y 11 z "hola"))))))

(deftest evaluar-escalar-1-test
  (testing "evaluar-escalar"
    (is (= (evaluar-escalar "chau" '(x 6 y 11 z "hola")) (list "chau" '(x 6 y 11 z "hola"))))))

(deftest evaluar-escalar-2-test
  (testing "evaluar-escalar"
    (is (= (evaluar-escalar 'y '(x 6 y 11 z "hola")) (list 11 '(x 6 y 11 z "hola"))))))

(deftest evaluar-escalar-3-test
  (testing "evaluar-escalar"
    (is (= (evaluar-escalar 'z '(x 6 y 11 z "hola")) (list "hola" '(x 6 y 11 z "hola"))))))

(deftest evaluar-escalar-4-test
  (testing "evaluar-escalar"
    (is (= (evaluar-escalar 'n '(x 6 y 11 z "hola")) (list (list (symbol ";ERROR:") 'unbound (symbol "variable:") 'n) '(x 6 y 11 z "hola"))))))

; evaluar-if

(deftest evaluar-if-0-test
  (testing "evaluar-if"
    (is (= (evaluar-if '(if 1 2) '(n 7)) (list 2 '(n 7))))))

(deftest evaluar-if-1-test
  (testing "evaluar-if"
    (is (= (evaluar-if '(if 1 n) '(n 7)) (list 7 '(n 7))))))

(deftest evaluar-if-2-test
  (testing "evaluar-if"
    (is (= (evaluar-if '(if 1 n 8) '(n 7)) (list 7 '(n 7))))))

(deftest evaluar-if-3-test
  (testing "evaluar-if"
    (is (= (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f"))) (list (symbol "#<unspecified>") (list 'n 7 (symbol "#f") (symbol "#f")))))))

(deftest evaluar-if-4-test
  (testing "evaluar-if"
    (is (= (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f"))) (list 8 (list 'n 7 (symbol "#f") (symbol "#f")))))))

(deftest evaluar-if-5-test
  (testing "evaluar-if"
    (is (= (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f"))) (list (symbol "#<unspecified>") (list 'n 9 (symbol "#f") (symbol "#f")))))))

(deftest evaluar-if-6-test
  (testing "evaluar-if"
    (is (= (evaluar-if '(if) '(n 7)) (list (list (symbol ";ERROR:") (symbol "if:") 'missing 'or 'extra 'expression '(if)) '(n 7))))))

(deftest evaluar-if-7-test
  (testing "evaluar-if"
    (is (= (evaluar-if '(if 1) '(n 7)) (list (list (symbol ";ERROR:") (symbol "if:") 'missing 'or 'extra 'expression '(if 1)) '(n 7))))))

; evaluar-define

(deftest evaluar-define-0-test
  (testing "evaluar-define"
    (is (= (evaluar-define '(define x 2) '(x 1)) (list (symbol "#<unspecified>") '(x 2))))))

(deftest evaluar-define-1-test
  (testing "evaluar-define"
    (is (= (evaluar-define '(define (f x) (+ x 1)) '(x 1)) (list (symbol "#<unspecified>") (list 'x 1 'f (list 'lambda '(x) '(+ x 1))))))))

(deftest evaluar-define-2-test
  (testing "evaluar-define"
    (is (= (evaluar-define '(define) '(x 1)) (list (list (symbol ";ERROR:") (symbol "define:") 'missing 'or 'extra 'expression '(define)) '(x 1))))))

(deftest evaluar-define-3-test
  (testing "evaluar-define"
    (is (= (evaluar-define '(define x) '(x 1)) (list (list (symbol ";ERROR:") (symbol "define:") 'missing 'or 'extra 'expression '(define x)) '(x 1))))))

(deftest evaluar-define-4-test
  (testing "evaluar-define"
    (is (= (evaluar-define '(define x 2 3) '(x 1)) (list (list (symbol ";ERROR:") (symbol "define:") 'missing 'or 'extra 'expression '(define x 2 3)) '(x 1))))))

(deftest evaluar-define-5-test
  (testing "evaluar-define"
    (is (= (evaluar-define '(define ()) '(x 1)) (list (list (symbol ";ERROR:") (symbol "define:") 'missing 'or 'extra 'expression '(define ())) '(x 1))))))

(deftest evaluar-define-6-test
  (testing "evaluar-define"
    (is (= (evaluar-define '(define () 2) '(x 1)) (list (list (symbol ";ERROR:") (symbol "define:") 'bad 'variable '(define () 2)) '(x 1))))))


(deftest evaluar-define-7-test
  (testing "evaluar-define"
    (is (= (evaluar-define '(define 2 x) '(x 1)) (list (list (symbol ";ERROR:") (symbol "define:") 'bad 'variable '(define 2 x)) '(x 1))))))

; evaluar-or

(deftest evaluar-or-0-test
  (testing "evaluar-or"
    (is (= (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))))

(deftest evaluar-or-1-test
  (testing "evaluar-or"
    (is (= (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))))

(deftest evaluar-or-2-test
  (testing "evaluar-or"
    (is (= (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (list 7 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))))

(deftest evaluar-or-3-test
  (testing "evaluar-or"
    (is (= (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (list 5 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))))

(deftest evaluar-or-4-test
  (testing "evaluar-or"
    (is (= (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))))

; evaluar-set!

(deftest evaluar-set!-0-test
  (testing "evaluar-set!"
    (is (= (evaluar-set! '(set! x 1) '(x 0)) (list (symbol "#<unspecified>") '(x 1))))))

(deftest evaluar-set!-1-test
  (testing "evaluar-set!"
    (is (= (evaluar-set! '(set! x 1) '()) (list (list (symbol ";ERROR:") 'unbound (symbol "variable:") 'x) '())))))

(deftest evaluar-set!-2-test
  (testing "evaluar-set!"
    (is (= (evaluar-set! '(set! x) '(x 0)) (list (list (symbol ";ERROR:") (symbol "set!:") 'missing 'or 'extra 'expression '(set! x)) '(x 0))))))

(deftest evaluar-set!-3-test
  (testing "evaluar-set!"
    (is (= (evaluar-set! '(set! x 1 2) '(x 0)) (list (list (symbol ";ERROR:") (symbol "set!:") 'missing 'or 'extra 'expression '(set! x 1 2))  '(x 0))))))

(deftest evaluar-set!-4-test
  (testing "evaluar-set!"
    (is (= (evaluar-set! '(set! 1 2) '(x 0)) (list (list (symbol ";ERROR:") (symbol "set!:") 'bad 'variable '1) '(x 0))))))