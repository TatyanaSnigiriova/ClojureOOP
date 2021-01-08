(ns nsu1.core
  (:require [nsu1.def-create-doc :refer :all])
  (:require [nsu1.command-query :refer :all])
  (:require [clojure.pprint :refer :all])
  (:require [clojure.test :refer :all])
)

; Задача - ввести собственное определение ключевого слова класс, определение методов
; Определение типов документов
; В самих классах будут только поля (пока без типов)
(def-doc-type :BaseDoc
  ; Parents
  ()
  ; Fields
  (
    :cnt1
    :cnt2
  )
  ; ToDo Validator
  ;(valid :cnt1 pos?)
  ; Init
  (init :cnt1 1 :cnt2 2)
)

doc-hierarchy
(println "Calling the macro def-doc-type")
(pprint (macroexpand  `(def-doc-type :DerivedDoc
  (:BaseDoc)
  (:cnt3)
  ; ()
  (init :cnt1 2 :cnt2 4 :cnt3 6))))
(println "\n\n")

(println "Result of the macro def-doc-type")
(pprint (macroexpand  (def-doc-type :DerivedDoc
  (:BaseDoc)
  (:cnt3)
  ; ()
  (init :cnt1 2 :cnt2 4 :cnt3 6))))
(println "\n\n")

; ToDo
; Я не совсем понимаю:
; :nsu1.command-query/fields #{:cnt1 :cnt2}
; Мы должны указывать полный квантификатор имени? И можем ли мы указывать разный уровень доступа для полей?
; : - public
; :: - private

(pprint (macroexpand (get-super-class-type :DerivedDoc)))

(defn -main []
  (println "-main")
  (println "end -main")
)

;Инстанцирование документов, вызов «методов»                ; new
(let [doc1 (create-doc :DerivedDoc :cnt1 1 :cnt2 2 :cnt3 3)]
  (println (get-value doc1 :cnt1) (get-value doc1 :cnt2))
  ;(inc-counters doc 2)
  (set-value doc1 :cnt1 55)
  (println  (get-value doc1 :cnt1) (get-value doc1 :cnt2))
)

(let [doc2 (create-doc :DerivedDoc :cnt3 3)]
  (println (get-value doc2 :cnt1) (get-value doc2 :cnt2) (get-value doc2 :cnt3))
  (set-value doc2 :cnt3 6)
  (println (get-value doc2 :cnt3))
  (println (document? doc2))
  (println (document? (apply hash-map [::type :Doc ::state (ref 1)])))
)


(println "mini-tests")
; BFS = ({:D} {:B :C} {:A :E}) представляет собой иерархию классов
(def-doc-type :A ()
  (:a1 :a2 :a3)
  (init :a3 42)
)

(def-doc-type :E ()
  (:e)
)

(def-doc-type :B (:A)
  (:b)
)
(def-doc-type :C (:E :A)
  (:c)
)
(def-doc-type :D (:B :C)
  (:d1 :d2)
)


(def-command m1)

(def-method m1 [(:A obj)]
            `(:A))

(def-method m1 [(:B obj)]
            (cons :B (super)))

(def-method m1 [(:C obj)]
            (cons :C (super)))

(def-method m1 [(:D obj)]
            (cons :D (super)))

(def-command m2)

(def-method m2 [(:A obj) msg]
            (list :A msg))

(def-method m2 [(:C obj) msg]
            (cons (list :C msg) (super (str msg "(after C)"))))

(def-method m2 [(:D obj) msg]
            (conj (super msg) (list :D msg)))

(def-method m2 [(:E obj) msg]
            (list :E msg))


(def d (create-doc :D :d1 1 :d2 2 :b 3 :c 4 :a1 5 :a2 7 :e 8))
(println "test-1")
(deftest test-1
   (testing "test-1"
      (is (= (m1 [d]) `(:D :B :C :A)))
      (is (= (m2 [d] "test") `((:D "test") (:C "test") :A "test(after C)")))
      (is (= 42 (get-value d :a3)))
      (is (do
            (set-value d :a3 24)
            (= 24 (get-value d :a3))))

      (is (= 7 (get-value d :a2)))))

(run-tests 'nsu1.core)
(println (m1 [d]))
(println `(:D :B :C :A))
(println (= (m1 [d]) `(:D :B :C :A)))
(println (= (m2 [d] "test") `((:D "test") (:C "test") :A "test(after C)")))
(println  (get-value d :a3))



