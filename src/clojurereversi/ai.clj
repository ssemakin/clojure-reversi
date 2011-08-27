(ns clojurereversi.ai
  (:use [clojurereversi.core :only (positions rival-color)])
  (:use [clojurereversi.gameplay :only (find-moves play-color end-of-game?)]))

(defn evaluate-game [color board]
  (count (positions color board)))

(defn minimax [color board depth]
  (let [moves (find-moves color board)
        rival-moves (find-moves (rival-color color) board)
        boards (map #(play-color color % board) moves)]
    (cond
      (or (<= depth 0) (end-of-game? moves rival-moves))
        (evaluate-game color board)
      (empty? moves)
        (- (minimax (rival-color color) board (dec depth)))
      :else
        (reduce max (vec (map #(- (minimax (rival-color color) % (dec depth))) boards))))))

(defn ai-find-best-move [color board depth]
  (defn find-best-score [scores]
    (when (seq scores) (reduce #(max-key last %1 %2) scores)))
  (let [moves (find-moves color board)
        boards (reduce merge (vec (map #(hash-map % (play-color color % board)) moves)))
        move-scores (->> (pmap #(hash-map % (- (minimax (rival-color color) (boards %) depth))) moves)
                      (vec) (reduce merge))
        best-score (find-best-score move-scores)]
    (println "moves:" move-scores)
    (println "best move:" best-score)
    (first best-score)))


(defn ai-play [color board]
  (ai-find-best-move color board 6))

