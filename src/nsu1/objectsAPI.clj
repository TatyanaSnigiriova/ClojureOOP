(ns nsu1.objectsAPI)

;Метаобъект – объект, который имеет операцию get instance
;Определение типа документов
;sections: (:fields (:f1) (:f2)) (:super :Base)
(defmacro def-doc-type [name & sections])
;Элементы интроспекции
(defn super-type [type])
(defn has-field? [type field])
;Поведение
(defn def-command [name])
(defn def-query [name])
(defn def-method [command-name type argv & body])

;инстанцирование
;fields: :f1 1 :f2 2 …
(defn create-doc [type & fields])
;доступ к полям (атрибутам)
(defn getf [obj field])
(defn setf! [obj field val])
;интроспекция (Только чтение (В отличии от рефлексии))
(defn doc-type [doc])
;вызов команды/запроса
;(полноценная функция)
(command-or-query doc & args)

