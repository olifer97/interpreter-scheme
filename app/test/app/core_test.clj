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



