(ns clojurereversi.main
  (:use [clojurereversi.boardb]))

(println (gen-board-type-args 2))
(def b-class (gen-board-type 2))
(println b-class)
(def b (new-obj (gen-board-type 2) 1 2 3 4 5 6 7))
(println b)
(println ((keyword (gen-cell-name 1 2)) b))
