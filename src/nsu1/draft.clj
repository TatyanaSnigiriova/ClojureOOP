(ns nsu1.draft)
(use 'clojure.set)

(def z (set ['a' 'b' 'c']))
(def x (set ['c' 'd' 'e' 'f']))

(println (seq (concat z x)))
(println (conj z x))
(println (cons z x))
(println (union z x))

