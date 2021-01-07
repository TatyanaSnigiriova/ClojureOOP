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
          ::type :Document
          ::super nil
          ::fields {}
          ::inits nil
         ; ToDo :: validators nil
        }))))



(defn init [& fields-values-array]
  "The function allows a user to fill in the init section of a class when defining it with def-doc-type."
  {
    :inits (
      apply hash-map fields-values-array
    )
  }
)


; Метод init просто упаковывает инициируемые поля в структуру нужного вида и действует довольно слепо -
; Все основные проверки, связанные с соответствием полей в :init и атрибутами :fields
; (как наследуемыми текущим классом, так и определёнными непосредственно в этом классе),
; а также валидация указанных начальных значений должна осуществляется в методе def-doc-type.

; Наследование реализовано через дерево-иерархии:
; Для текущего класса, наследующего атрибуты родительского класса, не требуется переопределять поля родительского класса:
; Они будут определены путем считывания по дереву-иерхии от родителя к наследникам. При этом наследник может
; инициализировать поля родительского класса, заменяя инициализацию в родительском классе.

(defn get-class-type [type]
  (get (get @doc-hierarchy type) ::type)
)

(defn get-class-fields [type]
  (get (get @doc-hierarchy type) ::fields)
)


(defn get-class-inits [type]
  (get (get @doc-hierarchy type) ::inits)
)

; Return only TypeName, not structure for superClass
(defn get-super-class-type [type]
  (get (get @doc-hierarchy type) ::super)
)


(defn get-all-fields [type]
  "Retrieves lazy-seq of all fields of the current class and its ancestor classes (Even if they are repeated)."
  (let [
      class-fields (get-class-fields type)
      supers (get-super-class-type type)
    ]
    (if
      (not (nil? supers))                                   ; Только базовый класс :Document может не иметь родителей
      (concat
        (apply concat
          (map
            #(get-all-fields %)
            supers
          )
        ) class-fields
      ) class-fields
    )))


(defn get-all-inits [type]
  "Retrieves all default values of the current class and its ancestor classes.
  The successor overrides values set in predecessor."
  (let [
      class-init (get-class-inits type)
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
      ) class-init                                          ;(merge {:a 1 :b 2 :c 3} {:b 9 :d 4})
                                                            ;;=> {:d 4, :a 1, :b 9, :c 3}
    )))


(defmacro def-doc-type [name supers fields & sections]
  "Adding information about the new class to the hierarchy if its ancestor exists
  (otherwise, the base class must be specified)"
  `(let [
    sections# (apply merge (map eval '~sections))
    inits# (get sections# :inits)
    supers# (if (empty? '~supers) (list :Document) '~supers)
    fields# (set '~fields)
    ; Так как текущего класса еще нет в иерархии, то конретно для него нельзя получить список всех полей,
    ; но их можно получить для его потомков, которые уже есть в иерархии
    all-fields# (seq
                    (if (not (nil? supers#))
                        (concat fields#
                          (apply concat
                            (map
                              #(get-all-fields %)
                              supers#
                            )
                          )
                        ) fields#
                    )
                  )
      unique-fields# (set all-fields#)
    ]
      (assert (not (contains? @doc-hierarchy '~name))
        (format "ERROR IN def-doc-type METHOD: Сlass name %s is already defined in doc-hierarchy" '~name)
      )
      (assert (= (count unique-fields#) (count all-fields#))
        (format "ERROR IN def-doc-type METHOD: Some field name for class %s is already defined in this class or in one of its parent classes" ~name)
      )
      (println inits#)
      (doseq [init-field# (keys inits#)]
        (assert (contains? unique-fields# init-field#)
          (format "ERROR IN def-doc-type METHOD: For class %s init-field %s not defined"  '~name init-field#)
        )
      )
      (dosync
        (alter
          doc-hierarchy
          (fn [h#]
            (assoc
              ;; private
              h# ~name {
                ::type   '~name
                ::super  supers#
                ::fields fields#
                ::inits   inits#
              }
            ))))))

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
    (println (str @state))
    {::type type,
     ::state state                                          ; ::fields будем использовать для списка всех полей
     ; Список полей для всех экземпляров одного класса будет одинаковым, поэтому его можно получить в doc-hierarchy
    }
  ))

(defn get-value
  "This is the getter for all class types"
  [obj field]
  (let [state @(obj ::state)]                               ; Мы храним все изменяемое состояние под одним ref
    (assert (contains? state field)
      (format "for an instance of an object of type %s
      - the object does not contain field '%s'." (str (obj ::type)) field
      )
    )
    @(state field)
  )
)

(defn set-value
  "This is the setter for all class types"
  [obj field new_value]
  (let [state @(obj ::state)]
    (assert (contains? state field)
      (format "for an instance of an object of type %s
              - the object does not contain field '%s'." (str (obj ::type)) field
      )
    )
    (dosync
      (ref-set (state field) new_value)
    )
  )
)