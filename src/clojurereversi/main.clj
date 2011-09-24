(ns clojurereversi.main
  (:use [clojurereversi.boardb]))

(def board-a (empty-board 2))
;(def board-b (assoc board-a (gen-cell-name-key 1 1) (clojurereversi.boardb.Cell. :white (:neighbours ((gen-cell-name-key 1 1) board-a)))))
(println board-a)
(println (put-white-stone board-a [1 1]))
(println (put-black-stone board-a [1 2]))
(println (white-stone? (put-white-stone board-a [1 1]) [1 1]))
(println (empty-cell? (put-black-stone board-a [1 1]) [1 2]))
