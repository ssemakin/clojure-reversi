(ns clojurereversi.cui
  (:use [clojurereversi.util :only (print-board)])
  (:use [clojurereversi.core :only (rival-color positions init-board)])
  (:use [clojurereversi.gameplay :only (find-moves play-color)])
  (:use [clojurereversi.ai :only (ai-play)])
  (:use [clojurereversi.stats :only (add-duration print-stats empty-stats)]))

(defn print-score [board]
  (let [whites (count (positions 'white board))
        blacks (count (positions 'black board))]
    (print "white:" whites "black:" blacks "/")
    (cond
      (> whites blacks) (println " white wins.")
      (< whites blacks) (println " black wins.")
      :else (println " draw.")))
  )

(defn user-input [color board]
  (print "$" color "goes to (row column, ex: 1 1) #> ")
  (flush)
  (let [valid-moves (find-moves color board)
        pos (vec (map #(Integer/parseInt %) (vec (.split #" " (read-line)))))]
    (if (valid-moves pos) pos
      (do
        (println "invalid move:" pos "," color "can go to:" valid-moves)
        (recur color board)))))

(defn console-ui [board color make-move stats]
  (let [all-moves (find-moves color board),
        all-moves-rival (find-moves (rival-color color) board)]
    (print-board board all-moves)
    (cond
      (and (empty? all-moves) (empty? all-moves-rival))
        (do (print-score board) (print-stats stats) (println "Game over."))
      (empty? all-moves)
        (do (println color "has no moves, passing over to" (rival-color color))
          (recur board (rival-color color) make-move stats))
      :else
      (do (let [start# (. System (nanoTime))
                pos (make-move color board)
                end# (. System (nanoTime))
                nstats (add-duration color (double (- end# start#)) stats)]
        (println)
        (recur (play-color color pos board) (rival-color color) make-move nstats))))))

(defn make-user-user-moves [color board]
  (({'white user-input, 'black user-input} color) color board))

(defn make-ai-user-moves [color board]
  (({'white ai-play, 'black user-input} color) color board))

(defn make-user-ai-moves [color board]
  (({'white user-input, 'black ai-play} color) color board))

(defn make-ai-ai-moves [color board]
  (({'white ai-play, 'black ai-play} color) color board))


; Let's start...
(console-ui (init-board 8) 'white make-ai-ai-moves empty-stats)
