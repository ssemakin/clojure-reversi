(ns clojurereversi.core)

(defn north-dir [pos] [(dec (pos 0)) (pos 1)])
(defn north-east-dir [pos] [(dec (pos 0)) (inc (pos 1))])
(defn east-dir [pos] [(pos 0) (inc (pos 1))])
(defn south-east-dir [pos] [(inc (pos 0)) (inc (pos 1))])
(defn south-dir [pos] [(inc (pos 0)) (pos 1)])
(defn south-west-dir [pos] [(inc (pos 0)) (dec (pos 1))])
(defn west-dir [pos] [(pos 0) (dec (pos 1))])
(defn north-west-dir [pos] [(dec (pos 0)) (dec (pos 1))])

(def directions [north-dir north-east-dir east-dir south-east-dir
                 south-dir south-west-dir west-dir north-west-dir])

(defn pos-middle-up-left [size] [(/ size 2) (/ size 2)])
(defn pos-middle-up-right [size] [(/ size 2) (+ (/ size 2) 1)])
(defn pos-middle-down-left [size] [(+ (/ size 2) 1) (/ size 2)])
(defn pos-middle-down-right [size] [(+ (/ size 2) 1) (+ (/ size 2) 1)])

(defn black? [board pos] (= (board pos) 'black))
(defn white? [board pos] (= (board pos) 'white))
(defn empty-cell? [board pos] (nil? (board pos)))
(defn cell-color? [color board pos] (= (board pos) color))

(defn put-black [board position] (conj board {position 'black}))
(defn put-white [board position] (conj board {position 'white}))
(defn put-color [color board position] (conj board {position color}))
(defn mput-color [color board positions]
  (if (first positions)
    (recur color (put-color color board (first positions)) (rest positions))
    board))

(def rival-color {'black 'white, 'white 'black})

(defn positions [color board]
  (->> (filter #(= (% 1) color) board) (map #(% 0))))

(defn valid-pos? [board pos]
  (and (> (pos 0) 0) (> (pos 1) 0)
    (<= (pos 0) (board :size)) (<= (pos 1) (board :size))))

(defn empty-board [size] (hash-map :size size))

(defn init-board [size]
  (-> (empty-board size)
    (put-white (pos-middle-up-left size))
    (put-black (pos-middle-up-right size))
    (put-black (pos-middle-down-left size))
    (put-white (pos-middle-down-right size)))
  )
