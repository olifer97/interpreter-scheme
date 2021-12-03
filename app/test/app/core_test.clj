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
