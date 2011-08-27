(ns mytest)

(defn put-black [board position] (conj board {position 'black}))
(defn put-white [board position] (conj board {position 'white}))
(defn put-color [color board position] (conj board {position color}))
(defn mput-color [color board positions]
  (if (first positions)
    (recur color (put-color color board (first positions)) (rest positions))
    board))

(defn pos-middle-up-left [size] [(/ size 2) (/ size 2)])
(defn pos-middle-up-right [size] [(/ size 2) (+ (/ size 2) 1)])
(defn pos-middle-down-left [size] [(+ (/ size 2) 1) (/ size 2)])
(defn pos-middle-down-right [size] [(+ (/ size 2) 1) (+ (/ size 2) 1)])

(defn empty-board [size] (hash-map :size size))

(defn print-board [board possible-moves]
  (print "  ")
  (loop [y 1]
    (when (<= y (board :size))
      (print y)
      (recur (inc y))))
  (println)
  (loop [y 1]
    (when (<= y (board :size))
      (print y "")
      (loop [x 1]
        (if (<= x (board :size))
          (do
            (cond
              (possible-moves [y x]) (print "*")
              (= (board [y x]) 'black) (print "@")
              (= (board [y x]) 'white) (print "0")
              :else (print "."))
            (recur (inc x)))
          (println)))
      (recur (inc y)))))

(defn init-board [size]
  (-> (empty-board size)
    (put-white (pos-middle-up-left size))
    (put-black (pos-middle-up-right size))
    (put-black (pos-middle-down-left size))
    (put-white (pos-middle-down-right size)))
  )

(defn north-dir [pos] [(dec (pos 0)) (pos 1)])
(defn north-east-dir [pos] [(dec (pos 0)) (inc (pos 1))])
(defn east-dir [pos] [(pos 0) (inc (pos 1))])
(defn south-east-dir [pos] [(inc (pos 0)) (inc (pos 1))])
(defn south-dir [pos] [(inc (pos 0)) (pos 1)])
(defn south-west-dir [pos] [(inc (pos 0)) (dec (pos 1))])
(defn west-dir [pos] [(pos 0) (dec (pos 1))])
(defn north-west-dir [pos] [(dec (pos 0)) (dec (pos 1))])

(defn valid-pos? [board pos]
  (and (> (pos 0) 0) (> (pos 1) 0)
    (<= (pos 0) (board :size)) (<= (pos 1) (board :size))))

(def rival-color {'black 'white, 'white 'black})
(def directions [north-dir north-east-dir east-dir south-east-dir
                 south-dir south-west-dir west-dir north-west-dir])

(defn black? [board pos] (= (board pos) 'black))
(defn white? [board pos] (= (board pos) 'white))
(defn empty-cell? [board pos] (nil? (board pos)))
(defn cell-color? [color board pos] (= (board pos) color))

(defn positions [color board]
  (->> (filter #(= (% 1) color) board) (map #(% 0))))

(defn find-state-in-dir [direction needed-state? start-pos capture-color board]
  (loop [s start-pos, captured {}]
    (let [next-pos (direction s)]
      (cond
        (= (board next-pos) capture-color)
          (recur next-pos (conj captured {:captured (concat (captured :captured) [next-pos])}))
        (and (needed-state? board next-pos) (valid-pos? board next-pos) (= (board s) capture-color))
          (conj captured {:covering-pos next-pos})
        :else {}))))

(defn find-move-in-dir [direction color start-pos board]
  (find-state-in-dir direction empty-cell? start-pos (rival-color color) board))

(defn find-moves [color board]
  (loop [poss (positions color board) moves #{}]
    (if (first poss)
      (let [xs (->>
                  (map #((find-move-in-dir % color (first poss) board) :covering-pos) directions)
                  (set)
                  (filter #(not-empty %)))]
        (recur (rest poss) (set (concat moves xs))))
      moves)))

(defn find-covering-stone-in-dir [direction color start-pos board]
    (find-state-in-dir direction #(cell-color? color %1 %2) start-pos (rival-color color) board))

(defn do-adjacent-for-stone [color pos board]
  (defn do-adjacent [color positions board]
    (if (first positions)
      (recur color (rest positions) (do-adjacent-for-stone color (first positions) board))
      board))

  (loop [dirs directions, result-board board]
    (if (first dirs)
      (let [nfound (find-covering-stone-in-dir (first dirs) color pos result-board)
            board-with-flipped (mput-color color result-board (nfound :captured))
            processed-board (do-adjacent color (nfound :captured) board-with-flipped)]
        (recur (rest dirs) processed-board))
      result-board)))

(defn play-color [color pos board]
  (do-adjacent-for-stone color pos (put-color color board pos)))


(defn end-of-game? [moves rival-moves]
  (and (empty? moves) (empty? rival-moves)))

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

(defn ai-find-best-move-dash [color board depth]
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
  (ai-find-best-move-dash color board 6))

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

(defn console-ui [board color make-move]
  (let [all-moves (find-moves color board), all-moves-rival (find-moves (rival-color color) board)]
    (print-board board all-moves)
    (cond
      (and (empty? all-moves) (empty? all-moves-rival))
        (do (print-score board)(println "Game over."))
      (empty? all-moves)
        (do (println color "has no moves, passing over to" (rival-color color))
          (recur board (rival-color color) make-move))
      :else
      (do (let [pos (make-move color board)]
        (println)
        (recur (play-color color pos board) (rival-color color) make-move))))))

(defn make-user-user-moves [color board]
  (({'white user-input, 'black user-input} color) color board))

(defn make-ai-user-moves [color board]
  (({'white ai-play, 'black user-input} color) color board))

(defn make-user-ai-moves [color board]
  (({'white user-input, 'black ai-play} color) color board))

(console-ui (init-board 8) 'white make-user-ai-moves)
