(ns clojurereversi.board
  (:use [clojurereversi.util]))

(def directions [north-dir north-east-dir east-dir south-east-dir
                 south-dir south-west-dir west-dir north-west-dir])

(defn pos-middle-up-left [size] [(/ size 2) (/ size 2)])
(defn pos-middle-up-right [size] [(/ size 2) (+ (/ size 2) 1)])
(defn pos-middle-down-left [size] [(+ (/ size 2) 1) (/ size 2)])
(defn pos-middle-down-right [size] [(+ (/ size 2) 1) (+ (/ size 2) 1)])

;(defn black? [board pos] (= (board pos) :black))
;(defn white? [board pos] (= (board pos) :white))
;(defn empty-cell? [board pos] (nil? (board pos)))
;(defn cell-color? [color board pos] (= (board pos) color))

(defmacro black? [board pos] `(= ~@(board pos) :black))
(defmacro white? [board pos] `(= ~@(board pos) :white))
(defn empty-cell? [board pos] (nil? (board pos)))
(defn cell-color? [color board pos] (= (board pos) color))

(defn put-black [board position] (conj board {position :black}))
(defn put-white [board position] (conj board {position :white}))
;(defn put-stone [color board position] (conj board {position color}))
(defmacro put-stone [color board position] `(conj ~board {~position ~color}))
(defn mput-color [color board positions]
  (if-let [pos (first positions)]
    (recur color (put-stone color board pos) (rest positions))
    board))


;(defn positions [color board]
;  (keep #(when (= (% 1) color) (% 0)) board))

;(defn positions [color board]
;  (keep (fn [[pos stone]] (when (= stone color) pos)) board))

(defmacro positions [color board]
  `(keep (fn [[pos# stone#]] (when (= stone# ~color) pos#)) ~board))

(defn empty-board [size] (hash-map :size size))

(defn init-board [size]
  (-> (empty-board size)
    (put-white (pos-middle-up-left size))
    (put-black (pos-middle-up-right size))
    (put-black (pos-middle-down-left size))
    (put-white (pos-middle-down-right size))))

