(ns clojurereversi.main
  (:use [clojurereversi.boardb]))

(println (gen-board-type-args 2))

(def b-class (gen-board-type 2))
(println b-class)

(def b (new-obj (gen-board-type 2) 1 2 3 4 5 6 7))
(println b)
(println ((keyword (gen-cell-name 1 2)) b))

(def d { :n [1 2], :ne [1 2], :e [3 4], :se [5 6], :s [7 8] })
(println (direction-type? clojurereversi.boardb.EastSector180 d))

(println (build-sector {:e [1 2] :se [3 4] :s [5 6]}))
;(println (build-sector {:e [1 2] :se [3 4] :s [5 6] :w [1 3]}))

(def nbrs (find-neighbours 3 [1 1]))
(println "neighbours for [1 1]:" nbrs "-->" (build-sector nbrs))
;(println (assoc (build-sector nbrs) :e 1))

(println (init-board-args 2))

(println (macroexpand-1 '(new-obj (gen-board-type 2) (init-board-args 2))))
(println (new-obj (gen-board-type 2) (init-board-args 2)))

(println (make-board 2))
