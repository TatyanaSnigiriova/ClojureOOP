(ns nsu1.command-query
  (:require [nsu1.def-create-doc :refer :all])
)

(declare get-classes-from-graphs)
(declare inc-inds)
(declare dec-inds)

(defmacro def-command
  "This macro declares a multimethod ~name."
  [name]
  `(let [
      primarytable# (ref {})
      aroundtable# (ref {})
      beforetable# (ref {})
      aftertable# (ref {})
    ]
    (defn ~name [objs# & args#]
      ;; When defining method name objs# is [(:A1 A2) (fn[] (...))] and args#
      ;; is empty or consists of one keyword.
      ;; When performing name objs# is [inst1 inst2] and args# is (arg1 arg_2 ...).
      (if (document? (first objs#))
        (let [                                              ; Стандартный вызов
            classes# (map instance-class-type objs#)
            BFS_graphs_not_uniq# (map get-full-BFS-graph-for-class classes#)
            ;; For each graph we make all its classes-vertices distinct
            BFS_graphs# (
              map
                (fn [graph#] (distinct (remove #(= % ::Document) graph#)))
                BFS_graphs_not_uniq#
            )
            max_inds# (map #(dec (count %)) BFS_graphs#)    ; Высота каждого дерева
            graphs_number# (count BFS_graphs#)
          ]
          (apply perform-effective-around
            (concat
              (list aroundtable# beforetable# primarytable# aftertable# BFS_graphs#
                    (repeat graphs_number# 0) max_inds# inc-inds objs#
              ) args#
            )
          )
          ;(apply perform-effective-command
          ; (concat
          ;   (list @vtable# (get-class-type obj#) obj#)
          ;   args#
          ; )
          ;)
        )
        (if (not (empty? args#))
          (let [support-type# (first args#)]
            (cond
              (= support-type# :around)
              (dosync (alter aroundtable# assoc (first objs#) (second objs#)))
              (= support-type# :before)
              (dosync (alter beforetable# assoc (first objs#) (second objs#)))
              (= support-type# :after)
              (dosync (alter aftertable# assoc (first objs#) (second objs#)))
              true (assert false "Incorrect type of support.")
            )
          )
          ;; Special call for method registration: adding a new version of the multimethod ~name to its virtual table.
          (dosync (alter primarytable# #(assoc % (first objs#) (second objs#))))
          ; Непосредственная регистрация производится через def-method
        )
      ))))

(defmacro def-method
  "This macro defines a particular version of the multimethod ~name."
  [name objs_and_args & body]
  (let [classes_objs (filter #(seq? %) objs_and_args)
        objs (map #(second %) classes_objs)
        classes (map #(first %) classes_objs)
        args (vec (map #(if (seq? %) (second %) %) objs_and_args))]
    (assert (= objs (distinct objs)) (str "Identical names of the arguments of method " name "."))
    `(~name ['~classes (fn ~args ~@body)])))

;; dummy for super that is defined each time in perform-effective-command using 'binding.
(def ^:dynamic super nil)

; Собственно, механизм вызова (диспетчерезации)
(defn perform-effective-command
  "perform-effective-command performs the multimethod whose virtual versions are all kept in vtable.
   It is recursive and can be called explicitly by a user with (super ...) construction."
  [vtable BFS_graphs indices indices_to inds-changer objs & args]
  (let [classes (get-classes-from-graphs BFS_graphs indices)] ; Просто получаем список всех классов
    (if (contains? vtable classes)                          ; И вызываем тот, который зарегистрирован в классе для текущего вхождения
                                                            ; Индекс показывает, на каком уровне мы сейчас находимся

      (binding [super                                       ; Определение динамического лексического контекста
                ; Переопределяет локально значение этой перременной на все, что находится ниже по стеку
                (if (= indices indices_to)
                  (fn [& x] (assert nil (str "(super) can not be called from a method if"
                                             "the classes of ALL its arguments are base.")))
                  (partial perform-effective-command vtable BFS_graphs
                           (inds-changer indices indices_to) indices_to inds-changer objs))]
        (dosync (apply (vtable classes) (concat objs args))))
      (apply perform-effective-command (concat (list vtable BFS_graphs
                                                     (inds-changer indices indices_to) indices_to inds-changer objs) args)))))

(defn perform-effective-before-after
  "Performs before/after support methods contained in vtable. They can not be called explicitly."
  [vtable BFS_graphs indices indices_to inds-changer objs & args]
  (let [graphs_number (.size BFS_graphs)
        classes (get-classes-from-graphs BFS_graphs indices)]
    (when (contains? vtable classes)
      (dosync (apply (vtable classes) (concat objs args))))
    (when (not (= indices (if (= inds-changer inc-inds) indices_to (repeat graphs_number 0))))
      (apply perform-effective-before-after (concat (list vtable BFS_graphs
                                                          (inds-changer indices indices_to) indices_to inds-changer objs) args)))))

(defn perform-effective-around
  "Performs support around-methods contained in aroundtable.
   Each next around-method can be called with (super).
   When all the around-methods are performed primary-methods can be called with (super)."
  [aroundtable beforetable primarytable aftertable
   BFS_graphs indices indices_to inds-changer objs & args]
  (let [classes (get-classes-from-graphs BFS_graphs indices)
        graphs_number (.size BFS_graphs)
        before-primary-after
        (fn [& args1]
          (when (not (empty? @beforetable))
            (apply perform-effective-before-after (concat (list @beforetable BFS_graphs
                                                                (repeat graphs_number 0) indices_to inc-inds objs) args1)))
          (let [result
                (apply perform-effective-command (concat (list @primarytable BFS_graphs
                                                               (repeat graphs_number 0) indices_to inc-inds objs) args1))]
            (when (not (empty? @aftertable))
              (apply perform-effective-before-after (concat (list @aftertable BFS_graphs
                                                                  indices_to indices_to dec-inds objs) args1)))
            result))]
    (if (not (empty? @aroundtable))
      (if (contains? @aroundtable classes)
        (binding [super
                  (if (= indices indices_to)
                    before-primary-after
                    (partial perform-effective-around
                             aroundtable beforetable primarytable aftertable BFS_graphs
                             (inds-changer indices indices_to) indices_to inds-changer objs))]
          (dosync (apply (aroundtable classes) (concat objs args))))
        (if (= indices indices_to)
          (apply before-primary-after args)
          (apply perform-effective-around
                 (concat (list aroundtable beforetable primarytable aftertable BFS_graphs
                               (inds-changer indices indices_to) indices_to inds-changer objs) args))))
      (apply before-primary-after args))))



(defn get-classes-from-graphs
  "Gets a partiular set of classes from BFS_graphs corresponding to the indices vector.
  Each index means a class position in a BFS-graph that is a hierarchy of classes."
  [BFS_graphs indices]
  (let [graphs_number (.size BFS_graphs)]
    (loop [i (dec graphs_number)
           classes '()]
      (if (< i 0)
        classes
        (let [graph (nth BFS_graphs i)
              index (nth indices i)]
          (recur (dec i) (conj classes (nth graph index))))))))

(defn inc-inds
  "Increments the indices vector. The min value of the vector is a vector of all nulls and the
  max value is a vector each element of which is a number of classes in a hierarchy minus one."
  [sub_inds sub_max_inds]
  (let [index (last sub_inds)
        max_index (last sub_max_inds)]
    (if (empty? sub_inds) '()
                          (if (< index  max_index)
                            (concat (drop-last sub_inds) [(inc index)])
                            (concat (inc-inds (drop-last sub_inds) (drop-last sub_max_inds)) [0])))))

(defn dec-inds
  "Decrements the indices vector. The min value of the vector is a vector of all nulls and the
  max value is a vector each element of which is a number of classes in a hierarchy minus one."
  [sub_inds sub_max_inds]
  (let [index (last sub_inds)
        max_index (last sub_max_inds)]
    (if (empty? sub_inds) '()
                          (if (> index  0)
                            (concat (drop-last sub_inds) [(dec index)])
                            (concat (dec-inds (drop-last sub_inds) (drop-last sub_max_inds)) [max_index])))))

