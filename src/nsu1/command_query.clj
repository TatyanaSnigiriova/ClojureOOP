(ns nsu1.command-query)

(use 'clojure.set)

(def doc-hierarchy (ref {}))
;аналог Object, не может быть определен штатно
(dosync
  (alter doc-hierarchy
    (fn [h]
      (assoc h
        :Document {
          ::type :Document,
          ::super nil
          ::fields {}
          ::init nil
        }))))

(defmacro def-doc-type [name supers fields & sections]
  (let [
      sections (apply merge (map eval sections))
      init (get sections :init)
    ]
    `(let [
      super# (if (empty? '~supers) (list :Document) '~supers)
      fields# (set '~fields)
    ]
    ; Добавляем запись о новом классе в иерархию
    (dosync
      (assert (not (contains? @doc-hierarchy ~name))
        (format "Forbidden new class name %s." ~name)
      )                                                     ; Обязательное наследование от базового класса
      (alter
        doc-hierarchy
        (fn [h#]
          (assoc
            h# ~name {
              ::type ~name
              ::super super#
              ::fields fields#
              ::init '~init
            }
          )))))))

(defn init [field value & map]
  "The function allows a user to fill in the init section of a class when defining it with def-class."
  {:init (apply hash-map field value map)}
)

; Вопросы: Инит работает довольно слепо:
; Что если инициализируемого поля просто не будет в списке полей для текущего типа?
; Если мы даём возможность задавать начальное значение для родительских полей, опять же, как мы должны проверять их наличие?

; Сейчас наследование как таковое не реализовано, тк требуется считывать дерево иерархии от родителя к наследникам, чтобы получить
; Весь набор полей и их начальных значений, которые, опять же, могут быть переопределены в наследниках