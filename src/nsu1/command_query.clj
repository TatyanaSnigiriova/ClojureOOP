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
         ; ToDo :: validators nil
        }))))

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
(defn get-super-class-type [type]
  (get (get @doc-hierarchy type) ::super)
)

(defn get-all-fields [type]
  "Retrieves all fields of the current class and its ancestor classes."
  (let [
      class-def (get @doc-hierarchy type)
      class-fields (get class-def ::fields)
      supers (get-super-class-type type)
    ]
    (if
      (not (nil? supers))                                   ; Только базовый класс :Document может не иметь родителей
      (concat
        class-fields
        (apply concat
          (map
            (fn [super] (get-all-fields super))
            supers
          )
        )
      ) class-fields
    )))

(defn get-all-inits [type]
  "Retrieves all default values of the current class and its ancestor classes.
  The successor overrides values set in predecessor."
  (let [
      class-def (get @doc-hierarchy type)
      class-init (get class-def ::init)
      supers (get-super-class-type type)
    ]
    (if
      (not (nil? supers))
      (merge
        (apply merge
          (map
            (fn [super] (get-all-inits super))
            supers
          )
        ) class-init
      )                                                     ;(merge {:a 1 :b 2 :c 3} {:b 9 :d 4})
                                                            ;;=> {:d 4, :a 1, :b 9, :c 3}
      class-init
    )))


(defmacro def-doc-type [name supers fields & sections]
  "Adding information about the new class to the hierarchy if its ancestor exists
  (otherwise, the base class must be specified)"
  (let [
      sections (apply merge (map eval sections))
      inits (get sections :init)
    ]
    `(let [
      supers# (if (empty? '~supers) (list :Document) '~supers)
      fields# (set '~fields)                              ; Это странно, но без ' в отладчике поля всегда пусты
      ; Так как текущего класса еще нет в иерархии, то конретно для него нельзя получить список всех полей,
      ; но их можно получить для его потомков, которые уже есть в иерархии

      all-fields# (seq
                    (if (not (nil? supers#))
                        (concat fields#
                            (apply concat
                            (map
                              (fn [super#] (get-all-fields super#))
                              supers#
                            )
                          )
                        ) fields#
                    )
                  )
      unique-fields# (seq (set all-fields#))
    ]
       (dosync
         (assert (not (contains? @doc-hierarchy ~name))
            (format "Forbidden new class name %s." ~name)
         )                                          ; Имя класса ещё не должно быть в иерархии
         (assert (= (count unique-fields#) (count all-fields#))
            (format "The field name for class %s is already defined in one of its parent classes %s %s" ~name unique-fields#  all-fields#)
         )
         ; ToDo - нужна проверка для init - могут быть перечислены поля, которых нет в all-fields#
         (alter
           doc-hierarchy
           (fn [h#]
             (assoc
               ;; private
               h# ~name {
                         ::type   '~name
                         ::super  supers#
                         ::fields fields#
                         ::init   '~inits
                         }
                  ; По сути, возможость отладки появляется только после успешного добавления в иерархию или при помощи macroexpand
                  ; Не совсем ясно, почему '~ без ' вызывает ошибку - значения вычисляются?
                  ; Разобраться - ToDo
                  )))))))

; ToDo Создание экземпляра класса

(defn create-doc [type & fields_values]
  "Creates an instance of the class."
  {:pre [(contains? @doc-hierarchy type)]}
  (let [
      fields_values (if (nil? fields_values) '() fields_values)
      all_fields (set (get-all-fields type))
      all_inits (get-all-inits type)
      fields_values_map (merge all_inits (apply hash-map fields_values))
      state (ref {})
    ]
    ; Нужна проверка на пересечение типов
    (dosync
      (doseq [kv fields_values_map]
        (if (contains? all_fields (first kv))
          (alter state
            #(assoc % (first kv) (ref (second kv)))         ;Используя ref - есть возможность упаковать validator вместе со значением
          )
          (assert false
            (format
              "Constructor error for an instance of an object of type %s
              - the object does not contain field '%s'." (str type) (first kv)
            )
          )
        )
      )
    )
    ; Таким образом, и сам объект является именяемым, и его поля
    ; Что позволяет исполнять, например, механизм агрегирования

    ; Сам объект
    ;;все состояние хранится под одним ref
    ;;можем использовать стандартный механизм валидации
    {::type type,
     ::state state                                          ; ::fields будем использовать для списка всех полей
     ; Список полей для всех экземпляров одного класса будет одинаковым, поэтому его можно получить в doc-hierarchy
    }
  ))

