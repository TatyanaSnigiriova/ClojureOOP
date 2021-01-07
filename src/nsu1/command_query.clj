(ns nsu1.command-query)

;Механизм вызова
;;«заглушка» для super
(def ^:dynamic super nil)
;;собственно, механизм вызова
(defn perform-effective-command [vtable eff-type obj & args]
  ;;dispatch ищет наиболее специфичный метод
  ;;возвращает пару [тип метод]
  (let
    [
     [d-type eff-fn] (dispatch vtable eff-type)
     d-super-type (super-type d-type)
     ]
    ;;определение динамического лексического контекста
    (binding
      [super
       (partial
         perform-effective-command
         vtable d-super-type obj
         )
       ]
      (dosync
        (apply eff-fn (cons obj args))
        ))))

;;определение метода
(defmacro def-method [command-name type argv & body]
  `(~command-name [
                   ~type
                   (fn ~argv ~@body)                                     ; Лямбда-выражение
                   ]
     ))


(defmacro def-command [name]
  ;;функция с состоянием
  `(let [vtable# (ref {})]
     (defn ~name [obj# & args#]
       (if (document? obj#)
         ;;стандартный вызов
         (apply perform-effective-command
                (concat
                  (list @vtable# (doc-type obj#) obj#)            ;Мутабельное состояние
                  args#
                  )
                )
         ;;специальный вызов: регистрация метода
         (dosync
           (alter vtable#
                  #(assoc % (first obj#) (second obj#))
                  )                                                 ; Регистрация производится через def-method
           )))))


