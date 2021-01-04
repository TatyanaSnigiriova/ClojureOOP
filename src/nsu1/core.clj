(ns nsu1.core
  (:require [nsu1.command-query :refer :all])
  (:require [clojure.pprint :refer :all])
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
  ; Init
  (init :cnt1 1 :cnt2 2)
)

doc-hierarchy
(println "Calling the macro def-doc-type")
(pprint (macroexpand  `(def-doc-type :DerivedDoc
  (:BaseDoc)
  (:cnt3)
  (init :cnt1 2 :cnt2 4 :cnt3 6))))
(println "\n\n")

(println "Result of the macro def-doc-type")
(pprint (macroexpand  (def-doc-type :DerivedDoc
  (:BaseDoc)
  (:cnt3)
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
