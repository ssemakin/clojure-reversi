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



;(defn capture-north [stone-pos color board captured]
;  (cond
;    (= (board stone-pos) color)
;      (capture-north (north-dir stone-pos) color board (conj captured {stone-pos color}))
;    (and (nil? (board stone-pos)) (valid-pos? stone-pos board))
;      (conj captured {:capture-move stone-pos} )
;    :else captured))

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


(defn ai-find-best-move [color board points minmax-color minmaxf max-depth]
  (defn switchf [f]
    (if (= f min-key) max-key min-key))
  (defn find-best-score [scores f]
    ;(when (= max-depth 10) (println "finding best of:" scores))
    ;(when (seq scores) (when (= [[3 4] 27] (reduce #(f last %1 %2) scores)) (println "depth:" max-depth "scores:" scores)))
    (if (seq scores) (reduce #(f last %1 %2) scores) [[0 0] 0]))
  (let [enemy-moves (find-moves (rival-color color) board)
        moves (find-moves color board)
        bs (reduce merge (vec (map #(hash-map % (play-color color % board)) moves)))
        ;scores (reduce merge (vec (map #(hash-map % (count (positions color (bs %)))) moves)))
        minmax-scores (reduce merge (vec (map #(hash-map % (+ points (count (positions minmax-color (bs %))))) moves)))
        best-minmax-score (find-best-score minmax-scores minmaxf)]
    (cond
      (and (empty? moves) (empty? enemy-moves))
        ;(do (print "+both are empty:" {[0 0] (+ points (count (positions minmax-color board)))} "+"))
        [[0 0] (+ points (count (positions minmax-color board)))]

      (empty? moves)
        (ai-find-best-move (rival-color color) board points minmax-color (switchf minmaxf) (dec max-depth))

      (<= max-depth 0)
        ;(do (print "+" color "hit" max-depth "with" best-minmax-score "+"))
        best-minmax-score

      :else
        (let [minmax-score (last best-minmax-score)
              games-raw (map #(hash-map % (last (ai-find-best-move (rival-color color) (bs %) minmax-score minmax-color (switchf minmaxf) (dec max-depth)))) moves)
              ;testp (println "depth:" max-depth games-raw)
              games (reduce merge (vec games-raw))
              best-move (find-best-score games minmaxf)
              ]
          ;(when (= max-depth 10) (println "boo!" (find-best-score games minmaxf)))
          ;(when (= max-depth 10) (println "best local:" minmax-score "local scores" minmax-scores))
          (when (= max-depth 10) (println "games:" games))
          (when (= max-depth 10) (println "best move:" best-move "min:"))
          ;{[0 0] 0}
          best-move
            ))))

(defn ai-play [color board]
  (first (ai-find-best-move color board 0 color max-key 10)))

;(defn do-adjacent-for-stone [color pos board]
;  (loop [dirs directions, result-board board]
;    (if (first dirs)
;      (let [nfound (find-covering-stone-in-dir (first dirs) pos result-board)
;            board-with-flipped (mput-color color result-board (nfound :captured))
;            processed-board (do-adjacent color (nfound :captured) board-with-flipped)]
;        (recur (rest dirs) processed-board))
;      result-board)))


;(defn play [board position color]
;  (conj board {position 'black}))

;(defn capture-dir [direction start-pos color board]
;  (loop [s start-pos, captured {}]
;    (let [next-pos (direction s)]
;      (cond
;        (= (board next-pos) color)
;          (recur next-pos (conj captured {next-pos color}))
;        (and (nil? (board next-pos)) (valid-pos? board next-pos) (= (board s) color))
;          (conj captured {:capture-move next-pos})
;        :else captured))))
;
;(defn capture [start-pos color board]
;  (->> (mapcat #(capture-dir % start-pos color board) directions) (filter #(not-empty %))))

;(defn find-moves [color board]
;  (mapcat #(capture % (rival-color color) board) (positions color board)))

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

(console-ui (init-board 4) 'white make-user-ai-moves)


;(print-board (init-board 4) {})
;(ai-find-best-move 'white (init-board 4) 0)


;(def b (-> (init-board 4) (put-black [2 2]) (put-white [1 2]) (put-white [4 2]) (put-white [2 4]) (put-black [3 3]) (put-black [3 4])))
;(print-board b)
;(println (find-covering-stone-in-dir north-dir [4 2] b))
;(print-board (do-adjacent-for-stone 'white [4 2] b))


;(println (init-board 4))
;(println (positions 'white (init-board 4)))
;(println ((init-board 4) :size))
;(print-board (-> (init-board 4) (put-black [2 2]) (put-white [1 2]) (put-white [4 2])))
;(println (find-covering-stone-in-dir north-dir [4 2] (-> (init-board 4) (put-black [2 2]) (put-white [1 2]) (put-white [4 2]))))
;(println (north-dir [3 2]))
;(println (capture east-dir [3 2] 'white (init-board 4) {}))
;(println (capture [3 2] 'white (init-board 4)))
;(println (find-moves 'black (init-board 4)))

;(def tboard (-> (empty-board 4) (put-black [3 2]) (put-black [2 3]) (put-white [3 1]) (put-white [1 3])))
;(print-board tboard)
;(println (find-moves 'black tboard))
;(println (find-move-in-dir north-dir [3 2] 'white (init-board 4)))
;(println (find-move-in-dir east-dir [3 2] 'white (init-board 4)))
