(ns nsu1.core
  (:require [nsu1.command-query :refer :all])
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

(def-doc-type :DerivedDoc
  (:BaseDoc)
  (:cnt2 :cnt3)
  (init :cnt1 2 :cnt2 4 :cnt3 6)
)



(defn -main []
  (println "-main")
  (println)
)
