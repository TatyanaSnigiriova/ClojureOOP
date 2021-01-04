(ns nsu1.command-query)

(use 'clojure.set)

(def doc-hierarchy
  "Tree of object types ready for instantiation"
  (ref {})
)

; Аналог Object, не может быть определен штатно
(dosync
  (alter doc-hierarchy
    (fn [h]
      (assoc h
        :Document {
          ::type :Document                                  ; ToDo Так ли он нам нужен?
                                                            ; Можно же условится, что ключ сам по себе будет являться типом
          ::super nil
          ::fields {}
          ::init nil
        }))))

(defmacro def-doc-type [name supers fields & sections]
  "Adding information about the new class to the hierarchy if its ancestor exists
  (otherwise, the base class must be specified)"
  (let [
      sections (apply merge (map eval sections))
      init (get sections :init)
    ]
    `(let [
         super# (if (empty? '~supers) (list :Document) '~supers)
         fields# (set '~fields)                              ; Это странно, но без ' в отладчике поля всегда пусты
      ]

      (dosync
      (assert (not (contains? @doc-hierarchy ~name))
        (format "Forbidden new class name %s." ~name)
      )                                                     ; Обязательное наследование от базового класса
      (alter
        doc-hierarchy
        (fn [h#]
          (assoc
            ;; private
            h# ~name {
              ::type '~name
              ::super super#
              ::fields fields#
              ::init '~init
            }
               ; По сути, возможость отладки появляется только после успешного добавления в иерархию или при помощи macroexpand
               ; Не совсем ясно, почему '~ без ' вызывает ошибку - значения вычисляются?
               ; Разобраться - ToDo
          )))))))

(defn init [field value & map]
  "The function allows a user to fill in the init section of a class when defining it with def-class."
  {:init (apply hash-map field value map)}
)

; ToDo init работает довольно слепо:
; Что если инициализируемого поля просто не будет в списке полей для текущего типа?
; Если мы даём возможность задавать начальное значение для родительских полей, опять же, как мы должны проверять их наличие?

; ToDo Сейчас наследование как таковое не реализовано, т.к. требуется считывать дерево иерархии от родителя к наследникам,
; чтобы получить весь набор полей и их начальных значений, которые, опять же, могут быть переопределены в наследниках.

; Return only TypeName, not structure for superClass
(defn get-super-class-type [class]
  (get (get @doc-hierarchy class) ::super)
)
