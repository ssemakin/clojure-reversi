(ns clojurereversi.main
  (:use [clojurereversi.boardb]))

(def board-a (empty-board 2))
;(def board-b (assoc board-a (gen-cell-name-key 1 1) (clojurereversi.boardb.Cell. :white (:neighbours ((gen-cell-name-key 1 1) board-a)))))
(println board-a)
(println (put-white board-a [1 1]))
(println (put-black board-a [1 2]))
