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