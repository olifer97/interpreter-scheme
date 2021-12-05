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

(deftest proteger-bool-en-str-0-test
  (testing "restaurar-bool"
    (is (= (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)"))) (symbol "(and (or #F #f #t #T) #T)")))))

(deftest proteger-bool-en-str-1-test
  (testing "restaurar-bool"
    (is (= (restaurar-bool (read-string "(and (or %F %f %t %T) %T)")) (symbol "(and (or #F #f #t #T) #T)")))))

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

